# Development Plan

Features enough for @nponeccop:

- Authentication by (a non-existent Haskell port of) [avz]
- Session cookie generation/storage (which?)
- Authorization/user profile persistence (which?)
- Pubsub (which?)
- Optional static HTTP server for development

[avz]: https://github.com/synrc/avz/blob/master/src/github.erl 

## AVZ port

Google and Facebook authentication methods in Version 1 as they don't require HTTP endpoints unlike Twitter or GitHub.

## PubSub

- broadcasting messages to channels
- must support subscriber metadata/enumeration, e.g. fetching logins of online users.
