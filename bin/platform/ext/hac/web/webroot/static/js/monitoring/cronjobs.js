var cronJobTable;

$(document).ready(function() {
	cronJobTable = $('#cronjobs').dataTable({
		"bPaginate" : false,
		"bStateSave" : true,
		"bFilter" : false,
		"columnDefs": [{     
		  "targets": '_all',
		  "render": $.fn.dataTable.render.text()
		}]
	});

	$('#abortJobs').click(function() {
        var token = $("meta[name='_csrf']").attr("content");

        var url = $('#abortJobs').attr('data-url');
		$.ajax({
			url : url,
			type : 'POST',
			headers : {
				'Accept' : 'application/json',
                'X-CSRF-TOKEN' : token
			},
			success : function(data) {
				debug.log(data);
				if(!$.isEmptyObject(data)) {
					var i = 0;
					var keys = [];
					var result = '<div class="box"><dl>';
					$.each(data, function(key, value) {
						keys [i++] = key;
						result += '<dt></dt>';
						if (value) {
							result += '<dd>Successfully set to "abortable"</dd>';
							
						} else {
							result += '<dd>CronJob is not "abortable"</dd>';
						}
					});
					result += '</dl>';
					$('#abortingResult').html(result);

					$('dl dt').each(function(){
					   $(this).text(keys[$('dl dt').index(this)]); 
					})
				}
			},
			error : hac.global.err
		});

	});
	updateCronJobs();
	setInterval("updateCronJobs();", 2000);
});
function updateCronJobs() {
    var token = $("meta[name='_csrf']").attr("content");
    var url = $('#cronjobs').attr('data-cronjobsLoadUrl');
	$.ajax({
		url : url,
		type : 'GET',
		headers : {
			'Accept' : 'application/json',
            'X-CSRF-TOKEN' : token
		},
		success : function(data) {
			debug.log(data);
			cronJobTable.fnClearTable();

			for(pos in data) {
				var cronJob = data[pos];
				cronJobTable.fnAddData([cronJob.cronJobCode, cronJob.jobCode, cronJob.progress]);
			}

		},
		error : hac.global.err
	});

}