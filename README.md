#Commands to add to the code base:

usethis::pr_init("evocative_branch_name")
do a small amount of work, restart R, make sure the changes work, commit locally.
usethis::pr_push()
usethis::pr_finish() #this deletes the remote branch, so no need to do it on github.
