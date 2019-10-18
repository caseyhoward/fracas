import PostgresPubSub from "@udia/graphql-postgres-subscriptions";
import { PubSub as PS } from "graphql-subscriptions";

export type PubSub = PostgresPubSub | PS;
