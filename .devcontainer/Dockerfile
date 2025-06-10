FROM ubuntu:22.04

# Install necessary tools, R with required packages and Python dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        r-base r-base-dev \
        r-cran-httr r-cran-jsonlite r-cran-r6 \
        python3 python3-pip \
        build-essential && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Pre-install Python dependencies used by the tests
RUN pip3 install --no-cache-dir requests pandas
