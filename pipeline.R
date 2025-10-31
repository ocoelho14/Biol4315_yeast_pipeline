# Download sample data
wget -nc ftp://ftp.sra.ebi.ac.uk/vol1/fastq/ERR153/001/ERR1539001/ERR1539001.fastq.gz
gunzip ERR1539001.fastq.gz

# Run sequali for quality/sanity check
sequali --outdir ./sequali_reports ERR1539001.fastq


# 