let program = "Hello, World!"

let server = Bun.serve({
    fetch: async (request, server) => {
        let success = 
            let success = Bun.Server.upgrade(server, request)
            switch success {
            | true => 
                Option.None
            | false => 
                Response.make("Hello World")->Option.Some 
            }
        
        let username =
            request
            ->Request.headers
            ->Headers.get("x-user-name")
            ->Option.getOr("Unknown user")
        
        Response.make(`Hello ${username}!`, ~options={status: 200})
    },
})

let port =
    server
    ->Bun.Server.port
    ->Int.toString

let hostName = server->Bun.Server.hostname

Console.log(`Server listening on http://${hostName}:${port}!`)