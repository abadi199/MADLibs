import { nicknames } from "memorable-moniker";

const headers = new Headers({
  "Content-Type": "application/json",
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "*",
  "Access-Control-Allow-Methods": "PUT, GET, OPTIONS",
});

export async function handleRequest(request: Request): Promise<Response> {
  var url = new URL(request.url);
  if (request.method === "GET") {
    if (url.pathname === "/") {
      return newRoute();
    }
    return load(url.pathname);
  } else if (request.method === "PUT") {
    return save(url.pathname, request);
  } else if (request.method === "OPTIONS") {
    return new Response(null, { headers });
  }

  return new Response(null, { status: 404, headers });
}

function newRoute() {
  const key = nicknames.next();
  return new Response(JSON.stringify(key), { headers });
}

async function load(path: string) {
  const key = path.substr(1);
  const text = await KV.get(key);
  return new Response(JSON.stringify(text), { headers });
}

async function save(path: string, request: Request) {
  const key = path.substr(1);
  const text = await request.json();
  await KV.put(key, text);
  return new Response(text, { headers });
}
