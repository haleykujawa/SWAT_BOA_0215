To install:
- Install boa:

    ```
    conda create -n boa
    conda activate boa
    conda install botorch -c pytorch -c gpytorch -c conda-forge
    pip install ax-platform
    pip install git+https://github.com/madeline-scyphers/boa.git
    ```

To install from develop:

    ```
    pip install git+https://github.com/madeline-scyphers/boa.git@develop
    ```

To update BOA:


    ```
    pip install -U git+https://github.com/madeline-scyphers/boa.git
    ```

or for develop


    ```
    pip install git+https://github.com/madeline-scyphers/boa.git@develop
    ```
 
You can install from other branches as well by replacing develop in the above commands with the name of the branch you wish to install from.

- With boa environment activated, update the environment with the r packages (from the repository's root directory):

    ```
    conda env update --name boa-swat --file env-rswat-update.yml
    ```
