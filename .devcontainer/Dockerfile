FROM rocker/r-ver:4.3.2

# Install Python and system build tools
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        python3 python3-pip build-essential && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN R -e "install.packages(c('httr', 'jsonlite', 'R6', 'rjstat'), repos='https://cloud.r-project.org')"

# Pre-install Python dependencies used by the tests
RUN pip3 install --no-cache-dir requests pandas
