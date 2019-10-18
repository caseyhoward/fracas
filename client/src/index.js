import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";
import { SubscriptionClient } from "subscriptions-transport-ws";
import ApolloClient from "apollo-client";
import { InMemoryCache } from "apollo-cache-inmemory";
import { WebSocketLink } from "apollo-link-ws";
import gql from "graphql-tag";

const graphqlSubscriptionUrl = "ws://192.168.1.7:4000";
// const graphqlHttpnUrl = "http://192.168.1.7:4000";

const client = new SubscriptionClient(graphqlSubscriptionUrl, {
  reconnect: true
});

const cache = new InMemoryCache();

const link = new WebSocketLink({
  uri: `ws://localhost:4000/`,
  options: {
    reconnect: true
  }
});

const apolloClient = new ApolloClient({
  networkInterface: client,
  cache,
  link
});

document.addEventListener("DOMContentLoaded", function() {
  let notifiers = [];
  const app = Elm.Main.init({
    node: document.getElementById("root"),
    flags: {
      viewport: {
        width: Math.max(
          document.documentElement.clientWidth,
          window.innerWidth || 0
        ),

        height: Math.max(
          document.documentElement.clientHeight,
          window.innerHeight || 0
        )
      }
    }
  });

  app.ports.createSubscriptions.subscribe(subscription => {
    console.log("createSubscriptions called with", subscription);
    notifiers = [subscription].map(operation => {
      apolloClient
        .subscribe({
          query: gql`
            ${operation}
          `
        })
        .subscribe({
          next(data) {
            console.log("subscribe", data);
            app.ports.gotSubscriptionData.send(data);
          },
          error(error) {
            console.log(error);
          },
          complete(x) {
            console.log(x);
          }
        });
    });
  });

  serviceWorker.unregister();
});
