export default {
  async queue(batch, environment) {
    for (const message of batch.messages) {
      const payload = JSON.parse(new TextDecoder().decode(message.body));
      await environment.MESSAGES.put(message.id, JSON.stringify(payload));
    }
    batch.ackAll();
  },

  async fetch(_request, environment) {
    const keys = await environment.MESSAGES.list();
    const messages = await Promise.all(
      keys.keys.map(async ({ name }) => JSON.parse(await environment.MESSAGES.get(name))),
    );
    return Response.json(messages);
  },
};
