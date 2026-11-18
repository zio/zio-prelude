// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const {themes: prismThemes} = require('prism-react-renderer');

const lightCodeTheme = prismThemes.github;
const darkCodeTheme = prismThemes.dracula;

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'zio-prelude-docs-website',
  // Docusaurus 3 rejects a `url` carrying a sub-path; the path belongs in `baseUrl`.
  url: 'https://zio.dev',
  baseUrl: '/zio-prelude-docs-website/',
  onBrokenLinks: 'throw',
  markdown: {
    // `.md` is parsed as CommonMark, `.mdx` as MDX.
    //
    // Docusaurus 2 used @mdx-js/mdx v1, which tolerated bare `<` and `{` in prose. v3 treats
    // them as JSX, so a line like "FS2 <-> ZStream conversions" becomes a hard build error.
    // zio-sbt-website compiles docs with mdoc, which emits `.md`, and those files are prose
    // written for CommonMark rather than MDX. Parsing them as CommonMark keeps the upgrade from
    // breaking existing documentation; a project that wants JSX can still use `.mdx`.
    format: 'detect',
    hooks: {
      onBrokenMarkdownLinks: 'throw',
    },
  },
  favicon: 'img/favicon.png',

  organizationName: 'zio', 
  projectName: 'zio-prelude-docs-website',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },
  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      {
        docs: {
          id: 'default',
          path: './docs',
          routeBasePath: '/',
          sidebarPath: require.resolve('./docs/sidebars.js'),
       },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
        blog: false,
      },
    ],
  ],
  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: 'zio-prelude-docs-website',
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
    }),
};

module.exports = config;
