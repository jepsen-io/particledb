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

## Running on AWS clusters

With the [AWS Marketplace Jepsen
product](https://aws.amazon.com/marketplace/pp/prodview-ykhheuyq5qdnq) (see
your Marketplace subscriptions list
[here](https://us-east-1.console.aws.amazon.com/marketplace/subscriptions/))
click "Launch new instance", then choose "Amazon EC2", and "Launch from EC2 Console". Choose version 0.3.11 and click "Launch from EC2".

Pick your favorite instance type (an m8i.xlarge should work fine), set the root
volume storage to something reasonable--say 64 GB, and click "Launch Instance".
Click the instance ID in the green notification that pops up, and copy the
"Public DNS" address of the instance.

Ensure you have a ParticleDB x68_64 binary in this test harness directory; say,
in `jepsen.particledb/builds/1.1.4`. Copy the test harness directory from your
local machine to the EC2 node like so:

```sh
rsync -avz --delete-after --hard-links --exclude jepsen.particledb/target --exclude jepsen.particledb/store jepsen.particledb admin@SOME_EC2_DNS_ADDRESS.compute-1.amazonaws.com:
```

Then log into the EC2 node via SSH:

```ssh
ssh admin@SOME_EC2_ADDRESS.compute-1.amazonaws.com
```

On that node, start a virtual cluster using Linux containers, and install a few prerequisites:

```sh
jepsen-containers-start &&
scripts/setup.sh
```

Then run some tests using the binary that you placed in the `builds` directory:

```sh
cd jepsen.particledb
lein run test-all \
  -n c1 \
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
