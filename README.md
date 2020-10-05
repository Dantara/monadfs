# monadfs

Pure functional distributed file system build on Haskell.

### Prerequisites

This project relies on the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/).

It is recommended to get Stack with batteries included by
installing [Haskell Platform](https://www.haskell.org/platform/).

## Build

To build this project simply run

```sh
stack build
```

This will install all dependencies, including a proper version of GHC

## Run

This app consist of multiple executable. 
You can run each one independently.

To start the client, run the following command:

``` 
stack exec monadfs-client
```

To start the name server, run the following command:

``` 
stack exec monadfs-name-server
```

To start the storage server, run the following command:

``` 
stack exec monadfs-storage-server
```

## File Structure

Here you can see simplified file structure of a project:

```
.
├── client
│   └── Main.hs
├── name-server
│   └── Main.hs
├── shared
│   ├── API
│   │   ├── NameServer.hs
│   │   └── StorageServer.hs
│   ├── API.hs
│   └── Lib.hs
├── storage-server
│   └── Main.hs
└── test
    └── Spec.hs

```

- Folders `client`, `name-server`, `storage-server` contain code required only for the client, the name server and for the storage server respectively.
- Folder `shared` contains code which can be imported to every executable.
- Folder `test` constains tests for a code.
