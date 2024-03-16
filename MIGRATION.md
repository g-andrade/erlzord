# Migration guide

Whenever there's an interface breaking change (a change in the project's major version),
required migration instructions will be detailed in this file.

## From [1.x] to [2.x]

### Update

- any calls to `erlzord:encode/2` and `erlzord:encode/4` with out-of-bounds values
as these will now throw an exception
