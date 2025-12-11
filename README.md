# R-data-analysis-BEPs

The purpose of R-data-analysis-BEPs is to process, analyze and plot WhereWeMove data

## Contributing to the project

![](.github/imgs/251126_Github_collab_workflow.jpg?raw=true)

### Are you contributing for the first time?

Then follow these steps:

1. In the upstream repository [WWM-housinggame-data-analysis/R-data-analysis-BEPs](https://github.com/WWM-housinggame-data-analysis/R-data-analysis-BEPs), create a [fork](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/fork-a-repo#forking-a-repository) of (i.e. a copy of) this repository in your own Github account.

2. Create copy of your Github fork repository in your local computer by [cloning your fork repository](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/fork-a-repo#cloning-your-forked-repository)

    > Note:
    > When cloning the Github fork repository via the terminal, you should be able to see the git remote connections to `origin` (Github fork repsoitory) and `upstream` (Github upstream repository) when running `git remote -v`:
    > ```
    > $ git remote -v
    > origin    https://github.com/YOUR-USERNAME/YOUR-FORK.git (fetch)
    > origin    https://github.com/YOUR-USERNAME/YOUR-FORK.git (push)
    > upstream  https://github.com/ORIGINAL-OWNER/ORIGINAL-REPOSITORY.git (fetch)
    > upstream  https://github.com/ORIGINAL-OWNER/ORIGINAL-REPOSITORY.git (push)
    > ``` 

3. Inside the main folder of your local fork, create a subfolder where your code should be stored, e.g. `mkdir scripts_studentname/`

4. Copy to `R-data-analysis-BEPs/scripts_studentname/` the content found within `R-data-analysis-BEPs/Scripts_vjcortesa/`

5. If you reuse one of the scripts copied from  `Scripts_vjcortesa/`, rename the copied version in `scripts_studentname/` by adding your username to the script name, e.g. script.R -> script_joaoxg.R

6. To save the changes to be sent to the Github fork, run the following commands in the terminal:

    ```
    git status
    git add .
    git commit -m "tell what you did"
    git push
    ```

    > Note:
    > Do not rename or modify any file within `Scripts_vjcortesa/`.
    > If you find any of those changes when running `git status`, you can undo them by running:
    > ```
    > git restore Scripts_vjcortesa/NAME-OF-FILE`
    > ```

7. If the changes you did are now saved in your Github fork repository, and if you want to update the upstream repository [WWM-housinggame-data-analysis/R-data-analysis-BEPs](https://github.com/WWM-housinggame-data-analysis/R-data-analysis-BEPs) with those changes, you can do so by doing a [pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request-from-a-fork) to push the changes from your fork to ([WWM-housinggame-data-analysis/R-data-analysis-BEPs](https://github.com/WWM-housinggame-data-analysis/R-data-analysis-BEPs)).

8. Workaround procedure
git add .
git commit -m "write message"
git push
 
there is a message asking for git pull
 
git pull
 
there is a message complaining about a conflict and merge needs to happen
 
git status
 
git add .
 
git commit
 
you are directed to the scary git file
 
just write :wq
 
you are back in the terminal
 
just right git push
 
### Have you already contributed before?

Then follow these steps to add your progress to the repository:

1. Make sure your Github fork repository is updated in the Github webpage by clinking on [Sync Fork -> Update Branch](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/syncing-a-fork#syncing-a-fork-branch-from-the-web-ui)

2. Open the terminal and run `git pull` inside the main folder of the local fork `R-data-analysis-BEPs/` to make sure your local fork is updated.

    > Note:
    > You should be able to see the git remote connections to `origin` (Github fork repsoitory) and `upstream` (Github upstream repository) when running `git remote -v` in your terminal:
    > ```
    > $ git remote -v
    > origin    https://github.com/YOUR-USERNAME/YOUR-FORK.git (fetch)
    > origin    https://github.com/YOUR-USERNAME/YOUR-FORK.git (push)
    > upstream  https://github.com/ORIGINAL-OWNER/ORIGINAL-REPOSITORY.git (fetch)
    > upstream  https://github.com/ORIGINAL-OWNER/ORIGINAL-REPOSITORY.git (push)
    > ``` 


3. To save the changes to be sent to the Github fork, run the following commands in the terminal:

    ```
    git status
    git add .
    git commit -m "tell what you did"
    git push
    ```

    > Note:
    > Do not rename or modify any file within `Scripts_vjcortesa/`.
    > If you find any of those changes when running `git status`, you can undo them by running `git restore Scripts_vjcortesa/NAME-OF-FILE`

4. If the changes you did are now saved in your Github fork repository, and if you want to update the upstream repository [WWM-housinggame-data-analysis/R-data-analysis-BEPs](https://github.com/WWM-housinggame-data-analysis/R-data-analysis-BEPs) with those changes, you can do so by doing a [pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request-from-a-fork) to push the changes from your fork to ([WWM-housinggame-data-analysis/R-data-analysis-BEPs](https://github.com/WWM-housinggame-data-analysis/R-data-analysis-BEPs)).
