<br />
<div align="center">
  <a href="https://elm-spa-saas.wilsonross.dev" target="_blank">
    <img src="src/static/img/logo.svg" alt="Logo" width="115" height="35" />
  </a>
  <br />
  <br />
  <br />
  <h3 align="center">SaaS style spa written in elm</h3>
  <p align="center">
    <a href="https://elm-spa-saas.wilsonross.dev" target="_blank"> Demo </a>
  </p>
  <br />
</div>

## About

<div>
  <br />
  <p>I initially started writing this project in <a target="_blank" href="https://www.typescriptlang.org/">TypeScript</a> using the <a target="_blank" href="https://nextjs.org/">Next.js</a> framework. A colleague recommended <a target="_blank" href="https://elm-lang.org/">Elm</a> to me last year, and I decided to try the functional paradigm. Everything in the project was rewritten. <a target="_blank" href="https://strapi.io/">Strapi</a> was initially chosen as a content management system to serve as an application back-end. During the change of languages, <a target="_blank" href="https://strapi.io/">Strapi</a> got dropped in favour of <a target="_blank" href="https://pocketbase.io/">PocketBase</a>.</p>
  <br />
  <p>The application is still under active development. Some features - account management, for example - are currently at a bare minimum. Furthermore, refactoring the app is a priority to improve code reusability and readability. I plan to add additional features to the back-end by modifying the <a target="_blank" href="https://pocketbase.io/">PocketBase</a> installation. These will include security improvements, such as adding CSRF token support and validation.</p>
  <br />
</div>

## Built With

<ul>
  <br />
  <li><a href="https://elm-lang.org/" target="_blank">Elm</a></li>
  <li><a href="https://webpack.js.org/" target="_blank">Webpack</a></li>
  <li><a href="https://postcss.org/" target="_blank">PostCSS</a></li>
  <li><a href="https://tailwindcss.com/" target="_blank">TailwindCSS</a></li>
  <br />
</ul>

## Getting Started

<div>
  <br />
  <p>
    The npm package Elm is included in the installation. The Webpack
    configuration also consists of an Elm loader. Additionally, the sample env
    file contains the URL to the production API and the necessary content
    management identifiers. Installing is simple as cloning using Git,
    installing with npm, and renaming the sample env file. The Pocketbase schema
    is included if you prefer to set it up with your back-end.
  </p>
  <br />
</div>

```bash
git clone https://github.com/wilsonross/elm-spa-saas.git
cd elm-spa-saas
cp .env.sample .env
npm ci
npm start
```

<br />

## Roadmap

<br />

- [x] Add cms page
- [x] Add search page
- [ ] Add account page
  - [x] Page template
  - [ ] Account management
- [ ] Add static pages

<br />

## Contact

<br />

Ross Wilson - [ross.wilson.190298@gmail.com](mailto:ross.wilson.190298@gmail.com)

<br />











