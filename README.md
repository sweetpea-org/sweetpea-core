SweetPea Core
=============

The repository hosts the backend server code for the SweetPea language. If you want to use the language, you're probably be looking for the [python package][pypi] ([Source][pygit]). If you intend to contribute, read on. 

## Overview

This codebase builds a Haskell server with a few different responsibilities:
* Constructing CNF formulas derived from a domain-specific definition.
* Generating non-uniformly-distributed solutions to a CNF formula via [CryptoMiniSat][cmsat].
* Generating approximately uniformly-distributed solutions to a CNF formula via [UniGen][ugen].

The server is not intended to be consumed directly by users; rather, the [frontend][pygit] relies on the server to perform the aforementioned tasks. Because UniGen requires Linux, and CryptoMiniSat must be built from source, we provide a [docker image][dimg] that contains all dependencies to facilitate consumption. The image embeds the server as well as its dependencies: CryptoMiniSat, UniGen, and Redis.

Local development of the server does not _strictly_ require Linux, unless you want to use the endpoints that rely on UniGen or build a new docker image.

The server supports an asynchronous API for requests that will take longer than a few seconds. Using these endpoints requires a Redis server to be running.

## Building the Server

1. Install a Haskell development environment. Ensure [`stack`][stack] is installed.
2. Install dependencies with `stack install`.
3. Build with `stack build`.
4. Start the server with `stack exec server`. You should see a message indicating the server has been started:

```
$ stack exec server
Spock is running on port 8080
```

## Running Tests

Run Haskell unit tests with `stack test`.

Additional tests can be run from the `test` directory by executing individual bash scripts in that directory, though some of them seem to have been in a broken state since the beginning.

## API

The server only presents a handful of endpoints:

* `GET /` - A healthcheck, responds with `200 OK` if the server is running.
* `POST /experiments/build-cnf` - (DEPRECATED) Receives a JSON request body describing an experimental design and responds with a DIMACS CNF representation.
* `POST /experiments/generate` - (DEPRECATED) Receives a JSON request body describing an experimental design and responds with samples generated via UniGen.
* `POST /experiments/generate-non-uniform` - (DEPRECATED) Receives a JSON request body describing an experimental design and responds with samples generated via CryptoMiniSat.
* `POST /experiments/jobs` - Asynchronous - Receives a JSON request body describing an experimental design and the type of job to execute. (To build a CNF, to generate non-uniform samples, to generate uniform samples) Responds with a job id.
* `GET /experiments/jobs/<job id>` - Asynchronous - Receives a job id in the request URI, and responds with a status object describing the status of the requested job. The status object indicates whether or not the job is complete, as well as its result if complete.

The deprecated endpoints are synchronous. The asynchronous endpoints are preferred due to the long-lived nature of these tasks for larger designs.

TODO: Document the structure of the request/response objects for each endpoint. (For now, contributors will need to read the code in either the frontend or the server)

[pypi]: https://pypi.org/project/sweetpea/
[pygit]: https://github.com/sweetpea-org/sweetpea-py
[cmsat]: https://github.com/msoos/cryptominisat
[ugen]: https://bitbucket.org/kuldeepmeel/unigen
[dimg]: https://hub.docker.com/r/sweetpea/server
[stack]: https://docs.haskellstack.org/en/stable/README/