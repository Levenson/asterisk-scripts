#!/usr/bin/awk -f

# Parse Asterisk CDR logs and transform to SKYTEL predefine format.

# Delete " symbol from string
function trim (string){
    gsub (/"/,"",string);
    return string;
}

# Check call destination type 
function check (channel){
    if ( length(channel) < 7 )
	return "int";
    else
	return "ext";
}

BEGIN {
    # CSV delimiter
    FS="\",\"";

    # line_stop=line_start+line_count;

    # Get file size
    # "du -m " ARGV[ARGC-1] " | cut -f 1" | getline size;
    
    for (i = 1; i < ARGC; i++) {
	if (ARGV[i] == "-v"){
	    verbose = 1 
	} else
	    break
	delete ARGV[i]
    }
}


{
    # Output record separator;  Default is newline;
    ORS="\n";

    # Account code
    accountcode=$1;

    # Source
    src=$2;

    # Destination
    dst=$3
    
    # Destination context
    dcontext=$4

    # Caller*ID
    clid=$5

    # Channel
    channel=$6

    # Destination Channel
    dstchannel=$7

    # Last Application
    lastapp=$8

    # Last Data
    lastdata=$9

    # Start Time
    start=$10

    # Answer Time
    answer=$11

    # End Time
    end=$12

    # Duration
    duration=$13

    # Billable seconds
    billsec=$14

    # Disposition
    disposition=$15

    # AMA Flags
    amaflags=$16

    # Unique ID
    uniqueid=$17

    # User field
    userfield=$18

    # CDR sequence number
    sequence=$19

    ##### Private #####
    
    sktext=$20


    OFS="\",\""

    if ( verbose == 1 ){
	print "==============================================================================="
	printf "%15s %s\n", "clid:", clid
	printf "%15s %s\n", "lastdata:", lastdata
	printf "%15s %s\n", "disposition:", disposition
	printf "%15s %s\n", "src:", src
	printf "%15s %s\n", "start:", start
	printf "%15s %s\n", "amaflags:", amaflags
	printf "%15s %s\n", "dst:", dst
	printf "%15s %s\n", "answer:", answer
	printf "%15s %s\n", "accountcode:", accountcode
	printf "%15s %s\n", "dcontext:", dcontext
	printf "%15s %s\n", "end:", end
	printf "%15s %s\n", "uniqueid:", uniqueid
	printf "%15s %s\n", "dstchannel:", dstchannel
	printf "%15s %s\n", "duration:", duration
	printf "%15s %s\n", "userfield:", userfield
	printf "%15s %s\n", "lastapp:", lastapp
	printf "%15s %s\n", "billsec:", billsec
	printf "%15s %s\n", "channel:", channel
	printf "%15s %s\n", "sequence:", sequence
	printf "%15s %s\n", "sktext:", sktext
    } else {
	# Sometimes dst field doesn't contain dst number.
	telephone = dst;
	if ( dst == "s" ){
	    "echo " dstchannel "| cut -d/ -f2 | cut -d- -f1 " | getline telephone;
	}
	print "\""uniqueid,start,check(src),src,check(dst),telephone,billsec,duration,disposition,userfield,trim(sktext)"\"";
    }
}

