## Setting Up Virtual Environments

- Virtual environment can help us to create an isolated or separate environment. This will help us to avoid conflicts in dependencies across projects. If you write pip freeze on your terminal you will see all the installed packages on your computer. If we use virtualenv, we will access only packages which are specific for that project.
- Open your terminal and install virtualenv

```bash
pip install virtualenv
```

- After installing the virtualenv package go to your project folder and create a virtual env.

```bash
virtualenv venv
```

- Activate the virtual environment.

```bash
source venv/bin/activate
```

- After you write the activation command, your project directory will start with venv.

- Now, lets check the available package in this project by writing pip freeze. You will not see any package.

- For example, we are going to do a small flask project.

```bash
pip install Flask
```

- Now, let's write pip freeze to see the install packages in the project.

- When you finish you should dactivate active project using `deactivate`.

- `venv` should be included to .gitignore.
