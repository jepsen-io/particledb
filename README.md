# ParticleDB Jepsen Tests

This is a work-in-progress test suite for [ParticleDB](https://particledb.ai/)
a Postgres-compatible database from Orkes.

## Usage

You'll need a [Jepsen
environment](https://github.com/jepsen-io/jepsen?tab=readme-ov-file#setting-up-a-jepsen-environment)
first. Then you can run, in this directory:

```
lein run test --nodes n1 -w internal-pdb --time-limit 10 --concurrency 2 --version builds/pdb-1.1.17
```

Which will install ParticleDB on node `n1`, and run a test of internal
transaction consistency for ten seconds, with two clients, using the ParticleDB
binary `builds/pdb-1.1.17`. It looks like ParticleDB's versioned downloads are
now inaccessible, so `--version 1.1.14` won't work for the time being.

You can also build a fat jar with `lein uberjar`, and run it with `./test.sh
test -w append ...`.

At present, these tests are only for single-node deployments, so you should
provide only one DB node.

The `compare` workload also requires the [uv package
manager](https://docs.astral.sh/uv/), for the Hegel generative testing library. On Debian, install it with:

```
curl -LsSf https://astral.sh/uv/install.sh | sh
```

## Walkthrough

To start an AWS cluster, see Jepsen's [AWS cluster
instructions](https://github.com/jepsen-io/jepsen#aws).

For the last few weeks the ParticleDB version-specific downloads have been
unavailable, so you'll need to ensure you have a ParticleDB x68_64 binary in
this test harness directory. I name mine after the versions, like
`jepsen.particledb/builds/1.1.4`. Once you have a binary, copy the test harness
directory from your local machine to the EC2 node like so:

```sh
rsync -avz --delete-after --hard-links \
  --exclude jepsen.particledb/target \
  --exclude jepsen.particledb/store \
  jepsen.particledb \
  admin@SOME_EC2_DNS_ADDRESS.compute-1.amazonaws.com:
```

Then log into the EC2 node via SSH:

```ssh
ssh admin@SOME_EC2_ADDRESS.compute-1.amazonaws.com
```

Then run some tests using the binary that you placed in the `builds` directory.
If you're running in Cloudformation, use `--username admin --node n1`; if
you're using a single VM with containers, use `--node `c1`.

```sh
cd jepsen.particledb
lein run test-all \
  --node c1 \
  --version builds/1.1.16 \
  --no-lazyfs \
  --nemesis none \
  --concurrency 10n \
  --time-limit 30
```

This is a quick run that will cycle through various workloads, transaction
modes, and WAL sync settings, without fault injection.

While these are running, you can also have the test harness serve the results of each test (from the `store/` directory) via a web server:

```sh
cd jepsen.particledb
lein run serve
```

The web interface will be available at `http://SOME_EC2_ADDRESS:8080`.

## Workloads

These workloads use the JDBC clients for Postgres and ParticleDB. Each comes in
two flavors based on the client protocol it uses, e.g. `append-pg` and
`append-pdb`.

- `append`: Transactions which append unique integers to `TEXT` lists, and try
  to read those lists.

- `wr`: Transactions which write unique integers and read them.

- `internal`: Transactions which perform inserts, updates, and reads, and
  validate that within a transaction, one observes values consistent with
  previous reads and writes.

These workloads set up ParticleDB and Postgres alongside one another, and compare the two.

- `compare`: Generates schemas and SQL statements and applies them, using a
  single client, to both ParticleDB and Postgres. Compares the two for
equivalence. Shrinks to (relatively) minimal counter-examples when the two
diverge.

### Compare

The compare workload generates random SQL statements and searches for
divergence between Postgres and ParticleDB. When it finds a divergence, it
attempts to *shrink* those SQL statements into smaller, simpler ones that still
reproduce the bug. Any divergence is called *interesting*, and the smallest
cases the workload finds are called *final*.

Bugs are categorized into various flavors, called *origins*; these are strings
like "different data" or "out of range". See the `origin` function for a full
list. You can hunt for a bug with a specific origin by regular expression by
passing (e.g.) `--compare-origin 'different (data|rows)'`. An unknown origin is
represented by the string 'unknown'.

The compare workload generates a pair of directories for interesting and final
cases: `interesting` and `final`. Within each are subdirectories for the
origins, then files like 'different data 123.txt'. These contain step-by-step
explanations of the queries performed, the result if both were equivalent, and
if ParticleDB and Postgres diverged, the results for each respectively. There
also follows a condensed version which can be copy-pasted into e.g. a `psql` shell
to reproduce the problem interactively.

## Interactive Reproducer

There's a silly hack here for playing around with ParticleDB. You can run:

```clj
lein run repro-watch n2 repro.sql
```

This will connect to ParticleDB and Postgres on `n2`, assuming the same sort of
setup that the test uses. It'll watch for changes in `repro.sql`, which
contains SQL statements, one per line. It submits those SQL statements
sequentially to both databases and for ParticleDB, using both the Postgres and
ParticleDB clients. Then it prints out the statements with how each client
responded to them, highlighting any divergences. This way you can edit the SQL
in your favorite text editor and get instant feedback.

An easy way to get an instance of Postgres and ParticleDB running is to run the
`compare` workload with `--leave-db-running`. You can also quickly deploy
various ParticleDB versions using `scripts/deploy`. I'm afraid these are
quick-and-dirty hacks and have a few things hardcoded for my environment, but
they may be useful regardless.

For example, here's a problem with NaN using ParticleDB 1.1.18.11.rc-1.

```
lein run repro-watch n9 repro.sql
...
DROP TABLE IF EXISTS t;
  :pdb-pdb [#:next.jdbc{:update-count 0}]
   :pdb-pg [#:next.jdbc{:update-count 0}]
       :pg [#:next.jdbc{:update-count 0}]
CREATE TABLE t (a real);
  :pdb-pdb [#:next.jdbc{:update-count 0}]
   :pdb-pg [#:next.jdbc{:update-count 0}]
       :pg [#:next.jdbc{:update-count 0}]
INSERT INTO t (a) VALUES ('NaN');
  :pdb-pdb [#:next.jdbc{:update-count 1}]
   :pdb-pg [#:next.jdbc{:update-count 1}]
       :pg [#:next.jdbc{:update-count 1}]
UPDATE t SET a = 5.0;
  :pdb-pdb [#:next.jdbc{:update-count 0}]
   :pdb-pg [#:next.jdbc{:update-count 0}]
       :pg [#:next.jdbc{:update-count 1}]
DELETE FROM t;
  :pdb-pdb [#:next.jdbc{:update-count 0}]
   :pdb-pg [#:next.jdbc{:update-count 0}]
       :pg [#:next.jdbc{:update-count 1}]
SELECT * FROM t;
  :pdb-pdb [{:a ##NaN}]
   :pdb-pg [{:a ##NaN}]
       :pg []

```


## License

Copyright © 2026 Jepsen, LLC

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
https://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
