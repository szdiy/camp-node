<?php
function available($f) {
  return ($f != "." && $f != "..");
  }

if ($_FILES["upfile"]["error"] > 0)
  {
    echo "Error: " . $_FILES["upfile"]["error"] . "<br />";
  }
 else
   {
     echo "Upload: " . $_FILES["upfile"]["name"] . "<br />";
     echo "Type: " . $_FILES["upfile"]["type"] . "<br />";
     echo "Size: " . ($_FILES["upfile"]["size"] / 1024) . " Kb<br />";
     echo "Stored in: " . $_FILES["upfile"]["tmp_name"];


     move_uploaded_file($_FILES["upfile"]["tmp_name"],
                        "upload/" . $_FILES["upfile"]["name"]);
     echo "Stored in: " . "upload/" . $_FILES["upfile"]["name"] ;

     echo "alright<br/>";
     $files = scandir("upload");
     $files = array_filter($files, "available");
     echo "begin<br/>";
     echo count($files) . "<br/>";
     if (count($files) > 3) {
       array_multisort(
                       array_map( 'filemtime', $files ),
                       SORT_NUMERIC,
                       SORT_ASC,
                       $files);

       echo $files[0];
       unlink("upload/" . $files[0]);
     }
     echo "done<br/>";
   }
?>
