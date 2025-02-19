CCAFE App
=======================================

This repository contains the `Shiny app <https://hendrickslab.cu-dbmi.dev/CCAFE/>`_ for the Case and Control
Allele Frequency Estimation (CCAFE) method. You can read more about the method `here
<https://wolffha.github.io/CCAFE_documentation/>`_ and the app here [link to the Github pages or your paper].

This `GitHub Page <https://ccafe-app.readthedocs.io/en/latest/>`_ serves to record decision making, management, and
development for the `Case and Control Allele Frequency Estimation <https://wolffha.github.io/CCAFE_documentation/>`_
public application. It includes a detailed proposal and revised documentaion that will be continuesly updated as the
project develops.

**Getting Started**

The latest version can only be run locally and is intended for testing purposes. In etiher installation case you will need to copy ``.env.TEMPLATE`` as ``.env`` and fill in any empty values with your information.

**Install via renv**

Run locally within RStudio by cloning the repository and running ``renv::restore()`` from within the RStudio terminal. Open the ``app.R`` file and click on *Run App*.

**Install via Docker**

Download `Docker <https://docs.docker.com/get-started/introduction/get-docker-desktop/>`_ and download all files from this repository onto your local machine. Included will be the script file ``run_app.sh``. This file can be executed from your terminal (``./run_app.sh``) to host the app within the Docker container. To access the site once the container is running, click the local host site here `CCAFE <http://localhost:3850/ccafe/>`_.


