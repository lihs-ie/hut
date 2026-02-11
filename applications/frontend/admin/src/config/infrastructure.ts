export const infrastructure = {
  event: {
    pubSub: {
      topicName: process.env.EVENT_PUBSUB_TOPIC_NAME || "events",
    },
  },
} as const;
