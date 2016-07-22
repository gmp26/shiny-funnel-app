##How to propose a change

Open an issue by clicking on `Issues` and then `New Issue`. The issue will be given a number which is useful when tracking other associated problems, and also when identifying the code change that finally fixes it. When committing a fix for (say) issue 1, write a commit message which includes the words `Fixes #1`. Github will then automatically close the issue and create hyperlinks between the commit and the closed issue.

##How to make a change
Avoid editing the master branch directly as that may cause merge conflicts which have to be resolved in the terminal. Instead, make edits in a new branch and then submit that to Github as a pull request:
* Create a new branch for the change here in github
  * Go to the project home page (https://github.com/gmp26/shiny-funnel-app), open the branch drop-down menu, and type the name of the new branch. e.g. `gmp26-patch-1` or `djs-patch-3`. Github will switch to show the newly created branch.
  * In RStudio, choose `Tools > Version Control > Pull Branches`. Once pulled, the new branch will appear in the Git panel `master` drop down list. Unfortunately, you need to use a shell to checkout the new branch - RStudio does not have a command to do this.
  * In RStudio, choose `Tools > shell` and checkout the new branch - `$ git checkout gmp26-patch-1`. All edits you make now in RStudio affect the new branch only.
  * As you make changes, the Git panel will list them for each file. Use the Diff tab to see changes in a selected file.
  * Tick `Staged`, and then `Commit` for any changes you want to commit. It's a good idea to commit whenever you are in a state you may want to revisit later - even if there are more changes to come. No need to Push changes to GitHub until you have something ready to go into the master branch.
* Push changes to Github, and create a Pull Request.
  * Click `Push` in the Git panel. Changes will make their way upstream to the tracked patch branch there.
  * Open a pull request on Github by clicking on `New pull request`. Provided some changes have been committed to the patch branch, a pull request will be created requesting an update to the master branch. (The base of the pull request should be `master`).
  * At this point you are done. Other collaborators will be informed of the pull request, and whoever is responsible will test it and accept it into the master branch.

## Cleaning up afterwards
* Once a pull request has been accepted or closed, delete the branch on Github.

* To tidy up locally so RStudio sees the change, say `git fetch -p` in a shell. You will end up on an updated master branch, and the patch branches will have disappeared.



