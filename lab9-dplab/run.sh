echo '
Tester.removeSeamsFile("images/hehe.jpg", "hehe50.jpg", 50);
Tester.removeSeamsFile("images/hehe.jpg", "hehe100.jpg", 100);
Tester.removeSeamsFile("images/hehe.jpg", "hehe200.jpg", 200);
(*Tester.removeSeamsFile("images/cove.jpg", "cove50.jpg", 50);
Tester.removeSeamsFile("images/gorilla.jpg", "gorilla50.jpg", 50);
Tester.removeSeamsFile("images/truck.jpg", "truck50.jpg", 50);
Tester.removeSeamsFile("images/cove.jpg", "cove100.jpg", 100);
Tester.removeSeamsFile("images/gorilla.jpg", "gorilla100.jpg", 100);
Tester.removeSeamsFile("images/truck.jpg", "truck100.jpg", 100);
Tester.removeSeamsFile("images/cove.jpg", "cove200.jpg", 200);
Tester.removeSeamsFile("images/gorilla.jpg", "gorilla200.jpg", 200);
Tester.removeSeamsFile("images/truck.jpg", "truck200.jpg", 200);*)
' | sml sources.cm 
