<?php
$url = "http://localhost:8000";
/*
curl sucks in php...
function get_url($url, $method, $postData="", $content_type="text/plain") {
    $ch = curl_init($url);
    curl_setopt($ch, CURLOPT_CUSTOMREQUEST, $method);
    curl_setopt($ch, CURLOPT_ENCODING, $content_type);

    //Encode as string...arrays won't encode properly
    foreach ($postData as $var) if (strpos($var, '@') === 0) {
        $postAsString = true;
    }

    $str = '';

    foreach ($postData as $key => $val) {
        $str .= '&' . $key . '=' . $val;
    }

    $postData = substr_replace($str, '', 0, 1);

    $output = curl_exec($ch);
    $status = curl_getinfo($ch, CURLINFO_HTTP_CODE);
    echo $status;
    if ($status !=  200) {
        throw new Exception(sprintf("Bad response code %d", $status));
    }

    if (substr($output, 0, 6) == "Error:") {
        throw new Exception($output);
    }
    curl_close($ch);
}
*/

function get_url($url, $method, $postData=array(), $content_type="text/plain") {
    $context = stream_context_create(array(
        'http' => array(
            'method'  => $method,
            'header'  => "Content-type: text/plain\r\n",
            'content' => http_build_query($postData),
            'timeout' => 5,
        ),
    ));
    $ret = file_get_contents($url, false, $context);
    $code = preg_match('/^200/', $http_response_header[0]);
    if ($code == array()) {
        throw new Exception(sprintf("Bad response code"));
    }
    if (substr($ret, 0, 6) == "Error:") {
        throw new Exception($ret);
    }
    return $ret; 
}

class Profile{
    function __construct() {
        global $url;
        $suffix = "/profile";
        $this->url = $url.$suffix;
    }
    function get($username) {
        $data = array(username, urlencode($username));
        $url = $this->url."?".implode($data, "=");
        $ret = get_url($url, 'GET');
        $profile = json_decode($ret, true);
        return json_decode($profile['profile'], true);
    }
    function create($username, $password, $profile) {
        $data = array("username"=>$username, 
                      "password"=>$password,
                      "profile"=>json_encode($profile));
        $content = get_url($this->url, "PUT", $data);
        return $content;
    }
    function update($username, $profile) {
        $data = array("username"=>$username,
                      "profile"=>json_encode($profile));
        $content = get_url($this->url, "POST", $data);
        return $content;
    }
    function delete($username) {
        $data = array("username"=>$username);
        $content = get_url($this->url, "DELETE", $data);
        return $content;
    }
}

class Password{
    function __construct() {
        global $url;
        $this->url = $url."/password";
    }
    function change_password($username, $password) {
        $data = array("username"=>$username,
                      "password"=>$password);
        $content = get_url($this->url, "POST", $data);
        return $content;
    }
}
class Auth{
    function __construct() {
        global $url;
        $this->url = $url."/auth";
    }
    function authenticate($username, $password, $application) {
        $data = array("username"=>$username,
                      "password"=>$password,
                      "application"=>$application
        );
        $content = get_url($this->url, "POST", $data);
        return $content;
    }
}
class Token{
    function __construct() {
        global $url;
        $this->url = $url."/token";
    }
    function authenticate($username, $token, $application) {
        $data = array("username"=>$username,
                      "token"=>$token,
                      "application"=>$application
        );
        $content = get_url($this->url, "POST", $data);
        return $content;
    }
    function delete($username, $application) {
        $data = array("username"=>$username,
                      "application"=>$application
        );
        $content = get_url($this->url, "DELETE", $data);
        return $content;
    }
    function new_token($username, $application) {
        $username = array("username", urlencode($username));
        $application= array("application", urlencode($application));
        $suffix = implode($username,"=")."&".implode($application,"=");
        $content = get_url($this->url."?".$suffix, "GET");
        return $content;
    }
}
