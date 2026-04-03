function decode_jwt --wraps=read\ -P\ \'Enter\ JWT:\ \'\ \|\ jq\ -R\ \'split\(.\)\ \|\ .\[1\]\ \|\ @base64d\ \|\ fromjson\' --description alias\ decode_jwt\ read\ -P\ \'Enter\ JWT:\ \'\ \|\ jq\ -R\ \'split\(.\)\ \|\ .\[1\]\ \|\ @base64d\ \|\ fromjson\'
  read -P 'Enter JWT: ' | jq -R 'split(.) | .[1] | @base64d | fromjson' $argv
        
end
