FROM ubuntu:22.04

# Installera nödvändiga verktyg och R
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        r-base r-base-dev \
        build-essential \
        && apt-get clean && rm -rf /var/lib/apt/lists/*
