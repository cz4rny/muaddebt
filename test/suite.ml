let () = Alcotest.run "MuadʾDebt" [
  "readme_variants", [
    Alcotest.test_case "no_readme"           `Quick Test_muaddebt.test_no_readme;
    Alcotest.test_case "empty_readme"        `Quick Test_muaddebt.test_empty_readme;
    Alcotest.test_case "no_dashboard"        `Quick Test_muaddebt.test_no_dashboard;
  ];
  "dashboard_placement", [
    Alcotest.test_case "dashbaord_at_top"    `Quick Test_muaddebt.test_dashboard_at_top;
    Alcotest.test_case "dashbaord_in_middle" `Quick Test_muaddebt.test_dashboard_in_middle;
    Alcotest.test_case "dashbaord_at_end"    `Quick Test_muaddebt.test_dashboard_at_end;
  ];
  "ripgrep", [
     Alcotest.test_case "ripgrep"            `Quick Test_ripgrep.test_strips_end_comment_marks
  ];
]
