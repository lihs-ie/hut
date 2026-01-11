import { infrastructure } from "@/config/infrastructure";
import { PubSubEventBroker } from "@/infrastructure/event";
import { PubSub } from "@google-cloud/pubsub";

const topic = new PubSub().topic(infrastructure.event.pubSub.topicName);

export const EventBrokerProvider = {
  pubSub: PubSubEventBroker(topic),
} as const;
