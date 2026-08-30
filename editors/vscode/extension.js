const path = require('path');
const vscode = require('vscode');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

let client;
let serverWatcher;
let restartPromise;

function settings() {
  const config = vscode.workspace.getConfiguration('sqlgg');
  return { command: config.get('serverPath'), args: config.get('serverArgs') };
}

function makeClient({ command, args }) {
  const server = { command, args, transport: TransportKind.stdio };
  const clientOptions = {
    documentSelector: [{ scheme: 'file', language: 'sql' }],
    outputChannelName: 'sqlgg',
  };
  return new LanguageClient('sqlgg', 'sqlgg', { run: server, debug: server }, clientOptions);
}

function watchServer({ command }) {
  serverWatcher?.dispose();
  serverWatcher = undefined;
  if (!path.isAbsolute(command)) return;
  const pattern = new vscode.RelativePattern(vscode.Uri.file(path.dirname(command)), path.basename(command));
  serverWatcher = vscode.workspace.createFileSystemWatcher(pattern);
  serverWatcher.onDidChange(restart);
  serverWatcher.onDidCreate(restart);
}

async function start() {
  const config = settings();
  watchServer(config);
  client = makeClient(config);
  try {
    await client.start();
  } catch (err) {
    client.outputChannel.appendLine(`could not start "${config.command}": ${err.stack || err}`);
    vscode.window.showErrorMessage(
      `sqlgg: could not start "${config.command}". Set sqlgg.serverPath to the sqlgg-lsp executable. (${err.message})`
    );
  }
}

async function stop() {
  serverWatcher?.dispose();
  serverWatcher = undefined;
  if (client?.isRunning()) await client.stop();
  client = undefined;
}

function restart() {
  restartPromise ??= stop().then(start).finally(() => { restartPromise = undefined; });
  return restartPromise;
}

function activate(context) {
  context.subscriptions.push(
    vscode.commands.registerCommand('sqlgg.restartServer', restart),
    vscode.workspace.onDidChangeConfiguration((e) => {
      if (e.affectsConfiguration('sqlgg')) return restart();
    })
  );
  return start();
}

function deactivate() {
  return stop();
}

module.exports = { activate, deactivate };
