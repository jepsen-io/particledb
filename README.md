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
manager](https://docs.astral.sh/uv/), for the Hegel generative testing library.

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
