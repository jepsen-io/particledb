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

## Workloads

- `append`: Transactions which append unique integers to `TEXT` lists, and try
  to read those lists.

- `wr`: Transactions which write unique integers and read them.

- `internal`: Transactions which perform inserts, updates, and reads, and
  validate that within a transaction, one observes values consistent with
  previous reads and writes.

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
