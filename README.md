# ParticleDB Jepsen Tests

This is a work-in-progress test suite for [ParticleDB](https://particledb.ai/) a Postgres-compatible database from Orkes.

## Usage

You'll need a [Jepsen
environment](https://github.com/jepsen-io/jepsen?tab=readme-ov-file#setting-up-a-jepsen-environment)
first. Then you can run, in this directory:

```
lein run test
```

Which will install ParticleDB and run a test on your default nodes (`n1`, `n2`,
`n3`, `n4`, and `n5`). See `lein run test --help` for various options.

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
