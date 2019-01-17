// Javascript - Painel Gerencial
// acrescimo de funcoes e personalizacoes

// esconder o painel até o loading completo
$(document).on('shiny:connected', function(event) {
  
});

$(window).on('load', function() {
	$("#ss-overlay").html('<div class="loader"></div>');
	$("#ss-overlay").fadeIn();	
	
	var query = window.location.search.substring(1);
	var url = 'https://pepe.prefeitura.sp.gov.br/logout.php?'+query;	
	$('.fa-sign-out').closest('li').find('a').attr('href',url);
	
});


$.getScript("//cdn.datatables.net/1.10.16/js/jquery.dataTables.min.js", function(){
	checkLoad();
	//$('#DataTables_Table_2').css({'font-size': '13px'});
	// datatable ranking semanal
	//$('#DataTables_Table_2').DataTable().destroy();
    //$('#DataTables_Table_2').DataTable({
	//	"language": { "url": "//cdn.datatables.net/plug-ins/1.10.16/i18n/Portuguese-Brasil.json" },
    //    "scrollX": true
    //});
});

function scrollDT2() {
	// datatable ranking semanal
	$('#DataTables_Table_2').DataTable().buttons().destroy();
	$('#DataTables_Table_2').DataTable().destroy();
    $('#DataTables_Table_2').DataTable({
		//"language": { "url": "//cdn.datatables.net/plug-ins/1.10.16/i18n/Portuguese-Brasil.json" },
        "scrollX": "Bfrtip",
		"dom": 'ftp',
		"destroy": true,
		"escape": true
    });
}

function checkLoad() {
    if($.fn.DataTable.isDataTable( '#DataTables_Table_0' ) == false) {
       window.setTimeout(checkLoad, 400);
    } else {
		$(".wrapper").css({'filter': '100','opacity': '1.0'});
		$("#ss-overlay").fadeOut();
		$("#ss-overlay").html('');
    }
}