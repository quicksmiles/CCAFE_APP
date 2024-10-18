Plan
*********

.. title:: GWAS Case/Control Data Processing with Python and R Integration

Project Overview
-------------------

**Title:**  
Case and Control Estimation Graphical User Interface - Data Harmonizing and Integration via Python

**Objective:**
The goal of this project is to create a web based user interface, where researchers can upload their GWAS summary statistics file in Variant Call Format (vcf) and recapitulate case and control allele frequencies (AF) in their dataset. The system will query gnomAD to pull and merge required fields needed to compute the case and contol allele frequencies, specifically superpopulation minor allele frequencies.
The gnomAD information will be merged with the researchers dataset based on "chromosome" and "dsposition" fields. Once merged, R methods that reconstruct allele frequencies for case and control will be applied to the dataset. After the resulting table is produced all data will be purged after processing to maintain privacy.

**Scope:**

The following outlines the scope of requirements and tools used, necessary for the minimal and primary functionality of this web based applicaiton.

* Users upload GWAS file from gnomAD build 38.
* User upload must contain "chromosome", "position", "number of cases", 
  "number of controls", "odds ratio (OR)" or "beta coefficient", 
  "allele frequency total (combined caase and control)" or "standard error (SE)"
* Hail queries gnomAD for superpopulation minor allele frequency data.
* Data merged on "chromosome" and "position."
* R methods are applied via `rpy2` or script execution.
* Data purging post-processing via Docker services or Google Dataproc.
* Docker Compose for deployment.
* Hosted on Google Cloud Platform (GCP) virtual machine.

1. Requirements
---------------

**Functional Requirements:**

* Users can upload a GWAS summary statistics VCF file that's on build GRCh38 via a web interface.
* The program will query gnomAD via Hail for superpopulation minor allele frequencies corresponding to the "chromosome" and "position" in the user's file.
* Data merging of the GWAS summary statistics file with the gnomAD superpopulation minor allele frequencies based on matching "chromosome" and "position".
* R methods are applied to the merged data from within Python.
* After processing, the user’s data is securely deleted.
* The system runs within Docker containers, including one for the web server and one for task management.

**Non-Functional Requirements:**

* **Security:** All user data must be purged after the R methods have been applied.
* **Scalability:** The system should handle large datasets from GWAS files and gnomAD datasets efficiently.
* **Resource Management:** Constrain resources using Docker to ensure efficient usage on GCP.

**Tech Stack:**

* **Programming Languages:** Python (Flask), R
* **Libraries/Tools:**
  * 'rpy2' or R scripts for integrating R within Python.
  * Hail for querying and merging gnomAD data.
  * Docker Compose for file system management and resource relationships amongst tasks.
* **Hosting:** Google Cloud Platform (GCP) Virtual Machine and Dataproc.

2. User Stories
---------------

* As a researcher, I want to upload a GWAS summary statistics file and receive a merged file with case and control allele frequencies so that I can perform further analysis efficiently.
* As a researcher, I want to recieve accurate and thoughfully reproduced GWAS fields so as not to propegate bad data.
* As a data scientist, I want an efficient system to apply R methods to large merged datasets directly from Python, allowing me to leverage both languages for analysis.
* As a security-conscious user, I want my uploaded data to be deleted after processing to ensure privacy.

3. System Architecture
-----------------------

**High-Level Components:**

* **Web Server (Flask):** Manages user uploads and handles requests. Runs in its own Docker container.
* **Hail for querying and merging:** Hail queries gnomAD for allele frequencies based on the "chromosome" and "position" variables. Merges this data with the user’s GWAS file.
* **R Method Execution (rpy2):** The merged data is passed to R methods via `rpy2` or script execution.
* **Task Queue (Celery/Redis):** Runs the long-running tasks (querying gnomAD, merging, R processing) in the background. Runs in a separate Docker container for scalability.
* **Docker Compose:** Orchestrates containers for web server and task queue.
* **Data Purge:** Ensures all data related to the user’s GWAS file is purged after processing.

**Data Flow:**

1. User uploads GWAS summary statistics VCF file.
2. Hail queries gnomAD and returns superpopulation minor allele frequencies.
3. GWAS data and gnomAD data are merged.
4. R methods are applied to the merged data using `rpy2` or an R script.
5. Processed results are returned with the ability to be downloaded, and the uploaded data is deleted.

4. Data Structures and Algorithms
---------------------------------

**Data Models:**

* **GWAS Summary File:** Key Variables: chromosome, position, number of cases, 
  number of controls, odds ratio (OR) or beta coefficient, 
  allele frequency total (combined caase and control) or standard error (SE)
* **gnomAD Data:** Key Variables: chromosome, position, superpopulation minor allele frequency, reference and alternate allele.
* **Merged Data:** Combines user uploaded GWAS data with gnomAD superpopulation minor allele frequencies based on "chromosome" and "position."

**Algorithms:**

* **Hail Querying:** Efficiently queries gnomAD superpopulation minor allele frequencies from the "HGDP + 1000 Genomes Dense Hail MatrixTable" dataset on gnomAD and merges on chromosome/position pairs.
  
  This is an initial query attempt to retrieve superpopulation MAF with the aid of ChatGPT

   .. code-block:: python

      import hail as hl

      # Initialize Hail
      hl.init()

      # Load the HGDP + 1KG dense MatrixTable from the gnomAD release
      mt = hl.read_matrix_table("gs://gcp-public-data--gnomad/release/3.1.2/mt/genomes/gnomad.genomes.v3.1.2.hgdp_1kg_subset_dense.mt")

      # Annotate rows (variants) with allele frequency (AF) for alternate alleles
      mt = mt.annotate_rows(allele_freqs = hl.agg.call_stats(mt.GT, mt.alleles))

* **Merging Algorithm:** Match GWAS records with gnomAD records based on chromosome/position pairs and allele.
* **R Methods:** R methods are applied via Python using `rpy2` or script execution.

5. Development Roadmap
----------------------

**Milestones:**

* **Phase 1: Project Setup**
  * Set up Flask web server for file uploads.
  * Set up Hail for querying gnomAD.
  * Set up the whole user interface.
* **Phase 2: Data Processing**
  * Implement data merging between GWAS file and gnomAD results.
  * Test R integration with `rpy2` or R script execution.
* **Phase 3: Docker and Deployment**
  * Dockerize the application using Docker Compose.
  * Deploy on GCP VM.
* **Phase 4: Final Integration**
  * Set up task queues using Celery/Redis.
  * Ensure data purging functionality.

**Timeline:**

* Phase 1: 2 weeks
* Phase 2: 3 weeks
* Phase 3: 2 weeks
* Phase 4: 1 week

System Overview
===============

The project involves building a system where users can upload GWAS summary statistics files, query allele frequencies from gnomAD using Hail, merge the data, and apply R-based methods within Python. The system architecture leverages task queues using Celery and Redis for handling asynchronous tasks and Docker Compose for resource-efficient deployment. The project will run on a Google Cloud Platform virtual machine.

Key Components
--------------

1. **Flask Web Server**  
   The web server handles incoming user requests and manages file uploads. Once a user uploads their GWAS summary statistics file, it passes off the processing work to Celery workers.

2. **Task Queue in System Architecture (Celery/Redis)**  
   The task queue plays a critical role in managing long-running tasks, such as querying gnomAD, merging large datasets, and applying R methods, in an asynchronous manner. This allows the web server to remain responsive while handling potentially slow operations in the background.

**Why Use a Task Queue?**  
In this project, querying gnomAD and merging large GWAS datasets can take considerable time, especially with larger files. Instead of blocking the web server while waiting for these tasks to complete, the task queue offloads them to a separate worker process. This ensures that the web server can continue to handle new user requests without being tied up by long operations.

**How Celery and Redis Work Together**

* **Celery:** Celery is a distributed task queue that allows Python functions to run asynchronously (in the background). It's used to handle heavy tasks (like data processing) outside the main web server process.  
  In this system, Celery will be responsible for:
  * Running the Hail queries against gnomAD.
  * Merging the GWAS and gnomAD data.
  * Invoking R scripts to process the merged data.
  * Deleting the user’s uploaded data after the task completes.

* **Redis:** Redis is used as a message broker for Celery. It stores messages (tasks) that need to be processed and ensures they are delivered to the right workers.  
  Redis will handle the communication between the web server and the Celery workers, managing task queues, and ensuring tasks are executed in the background.

**Flow of Task Execution**

1. **User Uploads GWAS File:**  
   The web server receives the GWAS file and immediately delegates the heavy-lifting tasks (e.g., querying gnomAD, merging, running R methods) to Celery.
2. **Celery Worker Picks Up the Task:**  
   The task (querying gnomAD, merging files, running R methods) is placed in the task queue (Redis). A Celery worker picks up the task and starts processing it in the background.
3. **Task Execution:**  
   The worker executes the task in the background, such as fetching data from gnomAD, merging it with the GWAS file, and applying R methods.
4. **Result Returned:**  
   Once the task is complete, the result is either stored temporarily or returned to the web server to be sent back to the user. Afterward, the user's data is purged for security.
5. **Data Purging:**  
   After the R methods are applied and the output is returned to the user, the worker deletes the user’s data from the system to ensure privacy.

R Method Execution via Scripts
==============================

The **integration of R methods** within Python will rely on either:

* **rpy2** (for direct R calls in Python).
* **Executing R Scripts** via subprocess calls from Python.

Why Use R Scripts?
------------------

Although rpy2 provides a direct interface to call R functions from Python, there are situations where:

* The R environment may be complex (e.g., specific R libraries that are easier to run in an isolated script).
* Certain tasks may be better organized in standalone scripts for better modularity, easier debugging, or reuse.

Steps to Implement R Scripts in Python:
=======================================

1. **Prepare the Merged Data:**  
   After merging the GWAS and gnomAD data, save the resulting data to a CSV or a similar format that can be passed to the R script.

2. **Trigger R Script Execution:**  
   Use Python’s ``subprocess`` module to call R scripts. Example:

   .. code-block:: python

      import subprocess
      result = subprocess.run(["Rscript", "my_script.R", "input_data.csv", "output_data.csv"], capture_output=True)

3. **Process the Output:**  
   After the R script completes, the resulting data can be loaded back into Python for further use or returned to the user.

4. **Data Cleanup:**  
   Ensure that after the R script finishes, both the input (uploaded data) and any temporary files are deleted from the system.
