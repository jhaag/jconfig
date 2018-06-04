#!/usr/bin/env bash

#=== Ease of Use ================================================================
# Make a directory and change into it at the same time
mkcd () {
    mkdir -p -- "$1" && cd -P -- "$1"
}

#=== Core =======================================================================
# Load custom configurations for a file passed in, and handle versioning
# $1: a string representing the custom configs to add
# $2: the path to the file that we are adding the config to
# $3: the comment prefix for the specific configuration file we are setting up
load_custom_config () {
    local CUSTOM_CONFIG_DELIMITER="$3=== CUSTOM CONFIGURATIONS ==="
    
    echo -e "Customizing $(basename $2):"

    # If the file doesn't exist, create it and fill it trivially with the data
    if [[ ! -e "$2" ]]; then
	echo -e "\t- Creating $(basename $2)..."

	echo -e "$CUSTOM_CONFIG_DELIMITER\n$1\n$CUSTOM_CONFIG_DELIMITER\n" >> "$2"
    # If the file does exist...
    else
	# Check to see if it already has custom configs, and strip them if they exist
	if [[ $(head -n 1 $2) =~ $CUSTOM_CONFIG_DELIMITER ]]; then
	    echo -e "\t\t- Saving non-custom configuration..."
	    
	    sed -i "" "1,/$CUSTOM_CONFIG_DELIMITER/ d" $2
	fi

	# Add the custom configurations with header guards to the file
	echo -e "\t\t- Adding updated custom configurations..."

	cat <<-EOF > $2
		$(echo -e "$CUSTOM_CONFIG_DELIMITER")
		
		$(echo -e "$1")

		$(echo -e "$CUSTOM_CONFIG_DELIMITER")
		$(cat $2)
		EOF
    fi

    echo -e "$(basename $2) customized.\n"
}
