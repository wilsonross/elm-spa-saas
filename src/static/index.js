import { Elm } from "../elm/Main";
import "./style.css";

const node = document.getElementById("main");
const apiUrl = process.env.APP_API_URL;

const app = Elm.Main.init({
  node: node,
  flags: {
    apiUrl: apiUrl,
  },
});


app.ports.setCookie.subscribe(setCookie);
app.ports.getCookie.subscribe((key) => {
  getCookie(key, parsedCookie => {
    app.ports.cookieReceiver.send(parsedCookie);
  });
});

function setCookie([key, token, daysUntilExpiry]) {
  const date = new Date();
  const msUntilExpiry = daysUntilExpiry * 24 * 60 * 60 * 1000;
  const secure = "SameSite=strict;Secure;path=/";

  date.setTime(date.getTime() + msUntilExpiry);

  document.cookie = `${key}=${token};Expires=${date.toUTCString()};${secure}`;
}

function getCookie(key, callback) {
  const name = `${key}=`;
  const decodedCookie = decodeURIComponent(document.cookie);
  const cookieComponents = decodedCookie.split(";");
  let token = "";

  cookieComponents.forEach((component) => {
    if (!component.includes(name)) {
      return;
    }

    token = component.split("=")[1];
  });

  callback([key, token]);
}
