// Worker

export default {
  async fetch(request, env) {
    return await handleRequest(request, env);
  }
}

async function handleRequest(request, env) {
  let id = env.COUNTER.idFromName("A");
  let obj = env.COUNTER.get(id);
  let resp = await obj.fetch(request.url);
  let count = await resp.text();

  return new Response("Durable Object 'A' count: " + count);
}

// Durable Object

export class Counter {
  constructor(state, env) {
    this.state = state;
  }

  // Handle HTTP requests from clients.
  async fetch(request) {
    // Apply requested action.
    let url = new URL(request.url);

    // Durable Object storage is automatically cached in-memory, so reading the
    // same key every request is fast. (That said, you could also store the
    // value in a class member if you prefer.)
    let value = await this.state.storage.get("value") || 0;

    switch (url.pathname) {
    case "/increment":
      ++value;
      break;
    case "/tic-tac-toe":
      let gameElm = await import('../dist/assets/server-63ae8903.js');
      console.log(gameElm);
      console.log(gameElm.init);
      console.log(Object.entries(gameElm));
      const game = gameElm.Elm.Server.init({
        flags: "Initial Message"
      });
      console.log({game});
      game.ports.giveState.subscribe((x) => {
        console.log({x});
      })
      game.ports.receiveAction.send('55');
      break;
    case "/decrement":
      --value;
      break;
    case "/":
      // Just serve the current value.
      break;
    default:
      return new Response("Not found", {status: 404});
    }

    // We don't have to worry about a concurrent request having modified the
    // value in storage because "input gates" will automatically protect against
    // unwanted concurrency. So, read-modify-write is safe. For more details,
    // see: https://blog.cloudflare.com/durable-objects-easy-fast-correct-choose-three/
    await this.state.storage.put("value", value);

    return new Response(value);
  }
}
