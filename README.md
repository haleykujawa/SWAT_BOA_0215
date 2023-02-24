To install:
- Install boa:

    ```
    conda create -n boa
    conda activate boa
    conda install botorch -c pytorch -c gpytorch -c conda-forge
    pip install ax-platform
    pip install -U "sqlalchemy<2.0"
    pip install git+https://github.com/madeline-scyphers/boa.git
    ```
- With boa environment activated, update the environment with the r packages (from the repository's root directory):

    ```
    conda env update --name boa-swat --file env-rswat-update.yml
    ```
