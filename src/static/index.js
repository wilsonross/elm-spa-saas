import { Elm } from "../elm/Main";

const node = document.getElementById("main");
const apiUrl = process.env.APP_API_URL;

Elm.Main.init({
  node: node,
  flags: {
    apiUrl: apiUrl,
  },
});
