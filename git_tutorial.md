
## Git+Github ##
### Getting started ###

Clone repository to your local computer (if not already done)

```
git clone https://github.com/natj/cronus.git
```

and keep it updated by pulling from Github

```
git pull
```

### Commits ###

Use `status` to see what files you have changed

```
git status
```

The changes can also be investigated more closely by using `git diff` that prints the differences explicitly

```
git diff <TIEDOSTO>
```

Add relevant files that are to be commited 

```
git add <file1> <file2> <file3>...
```

Wrap the files into commit with comment of what is done

```
git commit -m "COMMENT"
```

After this either make another commit or send them for everyone to see by pushing them

```
git push
```

### Advanced: Branches ###

The development can also be separated into different branches (default branch is master) in case of large changes.
To check your current branch, use

```
git branch
```

New branch can be created by 

```
git branch <BRANCH_NAME>
```

and to switch into this new branch use

```
git checkout <BRANCH_NAME>
```

After the major changes in this new branch are ready and you have wrapped them into commits, push them to github

```
git push origin <BRANCH_NAME>
```

and dont remember to switch back to master branch by `git checkout master`.
After this, the branch can be merged into the master branch by submiting a pull request in Github.
