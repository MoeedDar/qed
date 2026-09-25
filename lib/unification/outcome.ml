type status = Solved | Postponed | Failed
type t = { ctx : Context.t; status : status }

let solved ctx = { ctx; status = Solved }
let postponed ctx = { ctx; status = Postponed }
let failed ctx = { ctx; status = Failed }

let halt outcome continuation =
  match outcome.status with Failed -> outcome | _ -> continuation outcome.ctx
