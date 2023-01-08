import { Elm } from "../elm/Main";
import "./style.css";

const node = document.getElementById("main");
const apiUrl = process.env.APP_API_URL;

Elm.Main.init({
  node: node,
  flags: {
    apiUrl: apiUrl,
  },
});
