import { Elm } from "../elm/Main";
import "./style.css";

const node = document.getElementById("main");
const apiUrl = process.env.APP_API_URL;
const accountMessageId = process.env.APP_ACCOUNT_MESSAGE_ID;

const app = Elm.Main.init({
  node: node,
  flags: {
    apiUrl: apiUrl,
    accountMessageId: accountMessageId,
  },
});

app.ports.setSession.subscribe(([token, daysUntilExpiry]) => {
  setCookie("session", token, daysUntilExpiry);
});

app.ports.getSession.subscribe(() => {
  getCookie("session", (parsedCookie) => {
    app.ports.recieveSession.send(parsedCookie);
  });
});

function setCookie(key, token, msUntilExpiry) {
  const date = new Date();
  const secure = "SameSite=strict;Secure;path=/";

  date.setTime(date.getTime() + msUntilExpiry);

  document.cookie = `${key}=${token};Expires=${
    msUntilExpiry === 0 ? 0 : date.toUTCString()
  };${secure}`;
}

function getCookie(key, callback) {
  const name = `${key}=`;
  const decodedCookie = decodeURIComponent(document.cookie);
  const cookieComponents = decodedCookie.split(";");

  const [token, expiry] = parseCookieComponents(name, cookieComponents);

  callback([key, token, expiry]);
}

function parseCookieComponents(name, components) {
  let token = "";
  let expiry = 0;

  components.forEach((component) => {
    if (component.includes(name)) {
      token = component.split("=")[1];
      return;
    }

    if (component.includes("Expires")) {
      const expiryStr = component.split("=")[1];
      const futureDateMs = new Date(expiryStr).getTime();
      const currentDateMs = new Date().getTime();

      expiry = futureDateMs - currentDateMs;
    }
  });

  return [token, expiry];
}
