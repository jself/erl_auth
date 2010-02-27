<?php
require_once( 'erl_auth.php');
require_once(dirname(__FILE__) . '/simpletest/autorun.php');
class TestProfile extends UnitTestCase {
    function testProfClass() {
        $p = new Profile();
        $out = $p->create("testuser", "testpass", array("profile_item" => "blank"));
        $this->assertTrue($out == "Created");
        
        $profile = $p->get("testuser");
        $this->assertEqual($profile["profile_item"], "blank");
        
        $out = $p->update("testuser", array("profile_item" => "no_longer_blank"));
        $this->assertEqual($out, "Changed");
        

        $profile = $p->get("testuser");
        $this->assertEqual($profile["profile_item"], "no_longer_blank");

        $out = $p->delete("testuser");
        $this->assertEqual($out, "Deleted");


        try {
            $p->get("testuser");
            $this->fail("Except an error getting the user");
        }   
        catch (Exception $e) {
            $this->pass();
        }

    }
}

class TestAuth extends UnitTestCase {
    function setUp() {
        $p = new Profile();
        $p->create("testuser", "testpass", array());
    }

    function tearDown() {
        $p = new Profile();
        $p->delete("testuser", "testpass", array());
    }

    function testAuthClass() {
        $p = new Profile();
        $pas = new Password();
        $a = new Auth();
        try {
            $a->authenticate("testuser", "wrong_pass", "testing");
            $this->fail("Expected an error");
        } catch (Exception $e){
            $this->assertEqual($e->getMessage(), "Error: Bad Password");
        }
        $a->authenticate("testuser", "testpass", "testing");
        $this->assertEqual($pas->change_password("testuser", "newpass"), "Changed");
        $a->authenticate("testuser", "newpass", "testing");

        try {
            $a->authenticate("testuser", "testpass", "testing");
            $this->fail("expected and exception");
        } catch (Exception $e) {
            $this->assertEqual($e->getMessage(), "Error: Bad Password");
        }
    }
}

class TestToken extends UnitTestCase {
    function setUp() {
        $p = new Profile();
        $p->create("testuser", "testpass", array());
    }

    function tearDown() {
        $p = new Profile();
        $p->delete("testuser", "testpass", array());
    }

    function testTokenClass() {
        $t = new Token();
        $a = new Auth();

        $token = $a->authenticate("testuser", "testpass", "testing");
        $token2 = $a->authenticate("testuser", "testpass", "testing2");

        $this->assertEqual($t->authenticate("testuser", $token, "testing"), "Matched");
        $this->assertEqual($t->authenticate("testuser", $token2, "testing2"), "Matched");
        
        $new_token = $t->new_token("testuser", "testing");
        $t->delete("testuser", "testing2");

        $this->assertEqual($t->authenticate("testuser", $new_token, "testing"), "Matched");

        try {
            $t->authenticate("testuser", $token, "testing");
            $this->fail("Expected an exception");
        } catch (Exception $e) {
            $this->assertEqual($e->getMessage(), "Error: Invalid Token");
        }   

        try {
            $t->authenticate("testuser", $token2, "testing2");
            $this->fail("Expected an exception");
        } catch (Exception $e) {
            $this->assertEqual($e->getMessage(), "Error: Invalid Token");
        }   
    }
}
