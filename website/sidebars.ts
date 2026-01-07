import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  docsSidebar: [
    'getting-started',
    {
      type: 'category',
      label: 'SQL Reference',
      link: {
        type: 'generated-index',
        description: 'SQL language features supported by sqlgg',
      },
      items: [
        'sql/ddl',
        'sql/queries',
        'sql/parameters',
        'sql/expressions',
        'sql/nullability',
        'sql/cte',
        'sql/aggregation-nullability',
        'sql/metadata',
        'sql/literals',
        'sql/dialects',
      ],
    },
    {
      type: 'category',
      label: 'OCaml',
      link: {
        type: 'generated-index',
        description: 'OCaml-specific features and examples',
      },
      items: [
        'ocaml/traits',
        {
          type: 'doc',
          id: 'ocaml/specifics',
          label: 'Expressions',
        },
        'ocaml/json',
      ],
    },
  ],
};

export default sidebars;
