Contributing
============

`develop`: development happens here; submit PRs against this branch
`master` : contains versioned releases that can be referenced from other repos

Bugs
----
If you want to submit a *bug*, please search the existing issues and if an appropriate issue was found, then add a comment with your report there.
If nothing already existing found, open a new issue and choose the appropriate issue template. Make sure to label it as `:bug:`.

!WARNING! do not attach log files or other sensitive information to your bug report.

Workflow
--------

We maintain all work items in Github issues arranged in milestones. This ensures that everybody can follow the planned development.
For visualisation we use `ZenHub`.

Work items are selected weekly into the *Selected* column of the kanban board. These are the items that following the grand plan will be worked on during the current week.

Submitting PRs
--------------

Pull requests should be opened against `develop`.

a) Please describe the PR in as much detail as would be necessary to understand the code changes, and make sure the description is consistent with the changes introduced.

b) Checklist: this list helps to submit a high-quality PR. Please, read all items, follow them and check them after you have verified the actions have been been taken.

- [ ] compiles (`cabal new-clean; cabal new-build`)
- [ ] tests run successfully (`cabal new-test`)
- [ ] documentation added and created (`cd docs; nix-shell --run make`)
- [ ] link to an issue
- [ ] link to an epic
- [ ] add estimate points
- [ ] add milestone (the same as the linked issue)

