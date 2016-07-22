##How to propose a change

Open an issue by clicking on Issues and then New Issue. The issue will be given a reference number that can be referenced as the code is changed. For example, when committing a fix for issue 1, a commit message which says 'Fixes #1' will automatically close the issue and hyperlink the commit to the issue.

##How to make a change

Avoid editing the master branch directly as that may cause merge conflicts which have to be resolved in the terminal. Instead, make edits in a new branch and then submit that to Github as a pull request:
* Create a new branch for the change here in github
  * Go to the project home page (https://github.com/gmp26/shiny-funnel-app), open the branch drop-down menu, and type the name of the new branch. e.g. `gmp26-patch-1` or `djs-patch-3`. Github will switch to show the newly created branch.
  * In RStudio, choose `Tools > Version Control > Pull Branches`. Once pulled, the new branch will appear in the Git panel `master` drop down list. Unfortunately, you need to use a shell to checkout the new branch - RStudio does not have a command to do this.
  * In RStudio, choose `Tools > shell` and checkout the new branch:
  ```
  $ git checkout gmp26-patch-1
  ```
  Git will say something like:
  ```
  Branch gmp26-patch-1 set up to track remote branch gmp26-patch-1 from origin.
  Switched to a new branch 'gmp26-patch-1'
  ```
  the RStudio Git panel will switch branch to the new branch too.
  
  
* All edits you make in RStudio should now be committed to the new branch

