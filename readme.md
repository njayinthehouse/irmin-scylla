# Installation
Working with Irmin-scylla requires two installations:
- Installing scylla driver to communicate with OCaml programs
- Installing the scylla backend for the Irmin.

### Installing Scylla driver
Scylla DB is just a C++ implementation of Cassandra DB. So the drivers used for C* works for Scylla too, without any changes. We have used the C++ implementation of Datastax Cassandra driver which can be downloaded from:
https://github.com/datastax/cpp-driver
Instructions to build it can be found at: https://datastax.github.io/cpp-driver/topics/building/
Please note the following when building the libraries:
- It uses a system based library called `libuv`. It can be installed using `sudo apt-get install libuv1.dev`. Ref: https://stackoverflow.com/questions/42175630/how-to-install-libuv-on-ubuntu
- While installing Irmin-scylla, Dune would ask for the static libraries generated by the driver. For this we need to create a `.a` file which has all the libraries from the driver. To attain that we need to change a variable `CASS_BUILD_STATIC:BOOL` to `ON`. This is present in `CMakeCache.txt` file in the root folder. Alternatively, a cleaner way to do the same would be `cmake -DCASS_BUILD_STATIC=ON`.

To use the C* driver with OCaml, we can add it to opam libraries as an external stub.
Dune and other files for the same can be found at: https://github.com/prismlab/irmin-scylla/tree/master/irmin-master/src/libcassandra
The folder structure in the git repo is designed to accomodate the correct placing of the libraries. 
In my setup, I have named the directory containing the driver and other related files as `libcassandra` . It can be added to opam using:
`opam pin add libcassandra -k path < path to root folder>` (irmin-master is the root folder).

The installation may ask to first install `ctypes`, `ctypes-foreign`, and `posix-types`, if not already installed in the current switch. It can be installed directly from opam.

### Installing the Scylla backend
Installing the Scylla backend is quite straight forward, just like any other backend. Since the backend is supposed to interact with the external driver which is written in C++, we have a C stub to convert some of the basic types between OCaml and C/C++. Dune is modified to install this file along with few library dependencies which are required to connect with the driver. These are system based libraries.
Irmin-scylla can be installed using:
`opam pin add irmin-scylla -k path < path to root folder>`

### Setting up the scylla database
Currently I am using the containerized version of the scylla database, for which instructions can be found at: https://docs.scylladb.com/operating-scylla/procedures/tips/best_practices_scylla_on_docker/
Alternatively, it can be installed by compiling the source present at its github repo.
Scylla DB does not provide Light weight transactions in the main source currently. Hence to use it in the docker, use `--experimental 1` flag when creating the container. 
To create the cluster instructions are given in the above link. TLDR; If the first container is named as `lwt-scylla1` and uses LWT then using it as seed node, cluster can be started with `docker run --name lwt-scylla2  --hostname lwt-scylla2 -d scylladb/scylla --seeds="$(docker inspect --format='{{ .NetworkSettings.IPAddress }}' lwt-scylla1)" --experimental 1`. Further nodes can be added in the similar fashion.
This has been tried when all the containers are on the same machine. 

### Examples
This repository also contains a simple example for setting a key-value pair to database in one node and fetching it back from another. 
I have also checked-in one piece of code which I run on `utop` for testing purpose, just in case it comes handy.


### Note
- Since the stores are implicit, here I am not asking user to provide any names for the keyspace and tables. The code assumes the following configuration for the datastore:
```
CREATE KEYSPACE irmin_scylla WITH replication = {'class': 'SimpleStrategy', 'replication_factor': '3'};

CREATE TABLE irmin_scylla.atomic_write (
    key text PRIMARY KEY,
    value text);

CREATE TABLE irmin_scylla.append_only (
    key text PRIMARY KEY,
    value blob);
```

- Remember to include `c-flags` in dune when running the applications, as shown in the examples.
- Everything is tested on dune 2.1.1
