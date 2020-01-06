# Get Started

To get started, please simply fork this GitLab repository and
follow the structure, testing and submissions guidelines below. 
**Remember to change it to a private repo ASAP.**

# Repository Structure

### Define a compiler.json

Define the following properties in the compiler.json:

- authors: your school mail address

# Submit

First, make a commit and push your own codes. From the root of this repo, run (please run make clean or wirte your gitignore first, I don't have enough space to store all your excutables):

```shell
git add .
git commit -m"{Your commit message}"
git push origin master
```

Then add a tag to request grading on your current submission:

```shell
git tag {tagname} && git push origin {tagname}
```

Beware that all of your tag names should be distinguished among one project repo. Therefore, remember to use **a new tag name** `{tagname}` in each submission.

Every submission will create a new Gitlab issue, where you can track the progress.
