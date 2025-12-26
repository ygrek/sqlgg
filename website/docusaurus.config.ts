import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

// Allow override via environment variables for fork deployments
const organizationName = process.env.GITHUB_REPOSITORY_OWNER || 'ygrek';

const config: Config = {
  title: 'sqlgg',
  tagline: 'Type-safe SQL code generator',
  favicon: 'img/favicon.ico',

  future: {
    v4: true,
  },

  url: `https://${organizationName}.github.io`,
  baseUrl: '/sqlgg/',

  organizationName: organizationName,
  projectName: 'sqlgg',

  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          path: '../doc',
          sidebarPath: './sidebars.ts',
          editUrl: 'https://github.com/ygrek/sqlgg/tree/master/',
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    colorMode: {
      defaultMode: 'dark',
      respectPrefersColorScheme: true,
    },
    navbar: {
      title: 'sqlgg',
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'docsSidebar',
          position: 'left',
          label: 'Documentation',
        },
        {
          href: 'https://github.com/ygrek/sqlgg',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Documentation',
          items: [
            {
              label: 'Getting Started',
              to: '/docs/getting-started',
            },
            {
              label: 'SQL Reference',
              to: '/docs/sql/ddl',
            },
            {
              label: 'OCaml',
              to: '/docs/ocaml/traits',
            },
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'GitHub',
              href: 'https://github.com/ygrek/sqlgg',
            },
            {
              label: 'opam',
              href: 'https://opam.ocaml.org/packages/sqlgg/',
            },
          ],
        },
      ],
      copyright: `sqlgg — Type-safe SQL code generator. Built with Docusaurus.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['ocaml', 'sql', 'bash', 'lisp'],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
