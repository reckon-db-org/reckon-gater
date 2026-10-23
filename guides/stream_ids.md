# Stream Ids

Every event in a Reckon store lives in a stream, and the store accepts only two
shapes of stream id. This page is the one place that states them. Every write
surface (the BEAM API, the gateway's HTTP and gRPC, the Go, Python and .NET
clients) applies the same check, implemented once in `reckon_gater_stream_id`.

## The two accepted shapes

**A user stream** is the id your application chooses for an aggregate or any
stream of its own:

```text
<prefix>-<hex>

order-018f6a7b8c9d4e5f60718293a4b5c6d7
```

- `<prefix>` is 1 to 32 lowercase ASCII letters: no digits, no hyphens, no
  uppercase. It names what the stream is about (`order`, `account`, `sess`).
- `-` is exactly one hyphen.
- `<hex>` is exactly 32 lowercase hexadecimal digits, and nothing follows
  them (not even a newline): 128 bits, one UUID's worth.

**A system stream** is for operational streams the stack itself keeps, such as
stream links:

```text
$<namespace>:<name>

$link:high-value-orders
```

- `$` is mandatory.
- `<namespace>` is a lowercase letter followed by lowercase letters, digits or
  hyphens (`link`, `link-sub`).
- `:` is exactly one colon.
- `<name>` starts with a letter or digit and continues with letters, digits,
  `_`, `.` or `-`. It is meant to be read by people.

Ids that look reasonable but are rejected:

| id | why |
|---|---|
| `user-123` | the suffix is 3 characters, not 32 hex digits |
| `users-42` | same |
| `acc-123` | same |
| `Order-018f6a7b8c9d4e5f60718293a4b5c6d7` | uppercase prefix |
| `order-018F6A7B8C9D4E5F60718293A4B5C6D7` | uppercase hex |
| `018f6a7b8c9d4e5f60718293a4b5c6d7` | no prefix |
| `counter-counter-1` | two hyphens, and the suffix is not hex |

## Making one

Do not invent the hex by hand. On the BEAM, `reckon_gater_stream_id:new/1`
returns a fresh id whose suffix is a UUIDv7, so ids sort by creation time:

```erlang
Id = reckon_gater_stream_id:new(order).
%% <<"order-019d7a4f3c2a7d8c9e0f1234567890ab">>
```

In any other language, take a UUID (v7 if you have it, v4 otherwise), drop the
hyphens, lowercase it and put your prefix in front:

```text
uuid       019d7a4f-3c2a-7d8c-9e0f-1234567890ab
stream id  order-019d7a4f3c2a7d8c9e0f1234567890ab
```

To check an id before you send it:

```erlang
ok = reckon_gater_stream_id:validate(<<"order-018f6a7b8c9d4e5f60718293a4b5c6d7">>),
{error, malformed_user_id} = reckon_gater_stream_id:validate(<<"user-123">>).
```

## Aggregate ids are stream ids

evoq stores an aggregate's events in the stream named by its aggregate id, and
passes the id through unchanged. So an aggregate id must itself be a valid user
stream id: `account-0123456789abcdef0123456789abcdef` works, `acc-123` and a
bare 32-hex string do not. Generate it with `reckon_gater_stream_id:new/1` when
the aggregate is first created, and keep it.

## What a rejected id looks like

A write with a malformed id is refused before anything reaches storage, and the
error carries the rule it broke:

```erlang
{error, {invalid_stream_id, malformed_user_id, <<"user-123">>, Rule}} =
    reckon_gater_stream_id:check(<<"user-123">>).
```

`Rule` is a sentence naming the shape, an example, how to make an id, and a link
to this page. A store returns this error from the reckon-db release that builds
it with `check/1` (reckon-gater#5); reckon-db 5.11.13 and older return
`{invalid_stream_id, Reason, StreamId}`, which names the input but not the rule.
The gateway passes the store's error on: its HTTP surface returns it in the
`error` field, and its gRPC surface answers `INVALID_ARGUMENT`. Whether a gRPC
client also sees the reason as the status message depends on the gateway
version; one that sends only the status code logs the error on the server.

Reads apply the same rule: the store lays streams out by prefix and suffix, so
an id of any other shape names no stream it can hold.

## Why the rule is this strict

Before reckon-gater 2.2.0 the store accepted any `letters-hex` id, and shapes
like `a-0`, `Order-DEADBEEF` and `demo-1779045695` all made it in. They broke
log searches, projection grouping by prefix, and every tool that assumed one
id shape. A lowercase prefix and a fixed 32-hex suffix give every id the same
length class, 128 bits of uniqueness, and a prefix that is always the stream's
type, which is what the store's structural layout keys on
(`reckon_gater_stream_id:parts/1`).
