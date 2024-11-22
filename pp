//           SBI Core Banking Project, Hyderabad, India.          *
//*****************************************************************
//                                                                *
//  	           PROGRAM - PFMOBEnquiryService.Java                 
//                                                                *
//*****************************************************************
//                 P R O G R A M    H I S T O R Y                 *
//                                                                *
//   PROGRAMMER    :    DATE       :  SPR NO   :   COMMENTS       *
//----------------------------------------------------------------*
//Niharika Tammana : 15/10/2024    : 24090001  :  MICROSERVICES   *
//Naga Sai Ganesh  : 15/10/2024    : 24090001  :  MICROSERVICES   *
//----------------------------------------------------------------*

package com.tcs.bancs.microservices.services;

import java.io.IOException;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.ExecutionException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.sql.rowset.serial.SerialException;
import javax.validation.Valid;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.google.gson.Gson;
import com.tcs.bancs.microservices.aggregator.AggregationServiceImpl;
import com.tcs.bancs.microservices.config.CacheConfig;
import com.tcs.bancs.microservices.configuration.PropertyLoader;
import com.tcs.bancs.microservices.exception.RrnException;
import com.tcs.bancs.microservices.exception.SrcException;
import com.tcs.bancs.microservices.impl.MigenqDetailsRepositoryImpl;
import com.tcs.bancs.microservices.util.FrameworkConstants;
import com.tcs.bancs.microservices.java.dbconnection.DBConnection;
import com.tcs.bancs.microservices.java.txtm.Error_Description;
import com.tcs.bancs.microservices.jvm.scpf.JVMUTENV;
import com.tcs.bancs.microservices.jvm.scpf.JvmLsCallCode;
import com.tcs.bancs.microservices.jvm.scpf.JvmLsOptCode;
import com.tcs.bancs.microservices.jvm.scpf.JvmLsEnvRecordArea;
import com.tcs.bancs.microservices.reqbean.RequestBean_PFMOB;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

import java.util.concurrent.*;

@RestController
@Api(value = "Aggregation_API", description = " ", tags = { "Aggregation API" })
@RequestMapping("/")
@Cacheable
@CacheEvict
public class PFMOBEnquiryService {

//////////////////////////////////////////////////////////////////////////--------ENV VARIABLES STARTED--------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	
	@Autowired
	AggregationServiceImpl aggregationServiceImpl;

	@Autowired
	MigenqDetailsRepositoryImpl migDetails;;

	@Value("${UTENVCALS-BANCS-TRACE-STATE}")
	private String bancsTraceState;

	@Value("${UTENVCALS-MASTER-DQPTYPE}")
	private String masterDQType;

	@Value("${UTENVCALS-BANCS-HOST}")
	private String bancsHost;

	@Value("${UTENVCALS-FNS-SYSNUM}")
	private String fnsSysnum;

	@Value("${UTENVCALS-CTRL-SYSNUM}")
	private String ctrlSysnum;

	@Value("${UTENVCALS-DAY-SYSNUM}")
	private String daySysnum;

	@Value("${UTENVCALS-NIGHT-SYSNUM}")
	private String nightSysnum;

	@Value("${UTENVCALS-NON24H-SYSNUM}")
	private String non24hsum;

	@Value("${UTENVCALS-DEF-RMODE}")
	private String defRMode;

	@Value("${UTENVCALS-MASTER-DB-1}")
	private String masterDB1;

	@Value("${UTENVCALS-MASTER-DB-2}")
	private String masterDB2;

	@Value("${UTENVCALS-SERVICES-FLAG}")
	private String servicesFlag;

	@Value("${VC-MS-SERVICE-ID}")
	private String serviceid;
	
	@Value("${pf.response.outline.location}")
	private String PF_RES_FILE_PATH;
	
	@Value("${mob.response.outline.location}")
	private String MOB_RES_FILE_PATH;
	
	@Value("${UTENVCALS-INSPARAM-FLAG}")
	private String INSPARAM_FLAG;
	
	@Value("${UTENVCALS-INSPARAM-VALUE}")
	private String INSPARAM_VALUE;
	
//////////////////////////////////////////////////////////////////////////--------ENV VARIABLES ENDED--------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

	Logger logger = LoggerFactory.getLogger(PFMOBEnquiryService.class);
	
//////////////////////////////////////////////////////////////////////////--------PATH FILES READING STARTED--------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	
	String ErrorCodeMasterFilePath = CacheConfig.frameworkConfigProperties
			.getProperty(FrameworkConstants.LOOKUP_FILES_PATH);
	String sourceAuth = CacheConfig.frameworkConfigProperties.getProperty(FrameworkConstants.LOOKUP_FILES_PATH);
	String ipConfig = CacheConfig.frameworkConfigProperties.getProperty(FrameworkConstants.LOOKUP_FILES_PATH);
	Properties error = PropertyLoader
			.readPropertyFile(new String(ErrorCodeMasterFilePath + "/ErrorCodeMaster.properties"));
	Properties sId = PropertyLoader.readPropertyFile(new String(sourceAuth + "/SourceIdList.properties"));
	Properties ipprop = PropertyLoader.readPropertyFile(new String(ipConfig + "/IpConfigMaster.properties"));
	
//////////////////////////////////////////////////////////////////////////--------PATH FILES READING ENDED--------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	
	private final DBConnection dbconnection;
	private final Response responseObj;

	@Autowired
	public PFMOBEnquiryService(DBConnection dbconnection, Response responseObj) {
		this.dbconnection = dbconnection;
		this.responseObj = responseObj;
	}
	
	@InitBinder
	public void initBinder(WebDataBinder binder) {
		binder.setDisallowedFields(new String[] {});
	}
	
//////////////////////////////////////////////////////////////////////////-------- OBJECTS CREATION STARTED--------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	
	ErrorResponse 			errobj 		= new ErrorResponse();
	Error_Description 		errdesc 	= new Error_Description();
	Validate_Teller_Branch 	validate 	= new Validate_Teller_Branch();

	JVMUTENV 				jvmutenv 	= new JVMUTENV();
	JvmLsCallCode 			callCode 	= new JvmLsCallCode();
	JvmLsOptCode 			optcode 	= new JvmLsOptCode();
	JvmLsEnvRecordArea 		commonarea 	= new JvmLsEnvRecordArea();

	PFEnquiryService_Child 	pfchild 	= new PFEnquiryService_Child();
	MOBEnquiryService_Child mobchild 	= new MOBEnquiryService_Child();

	InputValidations 		inpval 		= new InputValidations();
	Gson gson = new Gson();
//////////////////////////////////////////////////////////////////////////-------- OBJECTS CREATION ENDED--------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
//queue fix
	  // Load Balancer Executor for managing concurrent requests
	private final ExecutorService singleThreadExecutor = Executors.newSingleThreadExecutor();
	private final BlockingQueue<Callable<ResponseEntity<Object>>> requestQueue = new LinkedBlockingQueue<>();
//queue fix
    
	@SuppressWarnings({ "unchecked", "rawtypes"})
	@ApiOperation(value = "Call PFMOB", notes = "PFMOB Service:This API is used to fetch Customer Details.")
	@PostMapping(value = { "/PFMOBEnq" }, produces = { "application/json" })
	@CrossOrigin()
	@JsonIgnoreProperties(ignoreUnknown = true)
	@Validated
	public ResponseEntity<Object> getCustomerDetails(@RequestBody(required = false) @Valid RequestBean_PFMOB reqbean,
			HttpServletRequest request, @RequestHeader HttpHeaders headers, HttpServletResponse response)
			throws SerialException, SQLException, IOException, RrnException, SrcException, InterruptedException, ExecutionException {

//queue fix
    // Prepare task to process the request
    Callable<ResponseEntity<Object>> task = () -> processOriginalRequest(reqbean.getReferenceNumber(), reqbean, request, headers, response);

    // Submit the task to the queue
    Future<ResponseEntity<Object>> future = submitToQueue(task);

    // Wait for task completion and return the result
    logger.info("future.get()" + future.get());
    String dbConnection_flag = dbconnection.getCBSDBConnection("04", "", "", "");
    return future.get();
}

 private Future<ResponseEntity<Object>> submitToQueue(Callable<ResponseEntity<Object>> task) {
    return singleThreadExecutor.submit(() -> {
        try {
            // Poll and execute the next task in sequence
            return task.call();
        } catch (Exception e) {
            logger.error("Error processing request", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error processing request");
        }
    });
}

    @SuppressWarnings({ "unchecked", "rawtypes"})
    private ResponseEntity<Object> processOriginalRequest(String refno, RequestBean_PFMOB reqbean, HttpServletRequest request,
                                        HttpHeaders headers, HttpServletResponse response) 
            throws SQLException, IOException, SerialException, RrnException, SrcException, InterruptedException, ExecutionException {
        
//        Connection conn = null;
//        String finalResponse;
//        String entryDate = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss:SSS").format(LocalDateTime.now());
//queue fix        
        
		response.setHeader("X-Content-Type-Options", "nosniff");
		response.setHeader("X-Frame-Options", "DENY");
		response.setHeader("Content-Security-Policy", "default-src 'self'");
		response.setHeader("X-XSS-Protection", "1;mode=block");
		
//////////////////////////////////////////////////////////////////////////-------- INITIAL VARIABLES--------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
		
		String IPAddress = request.getHeader("X-Forwarded-For");
		String remoteAddress = request.getRemoteAddr();
		String accept = request.getHeader("Content-Type");
		DateTimeFormatter entryDate1 = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss:SSS");
		LocalDateTime time = LocalDateTime.now();
		String entryDate = entryDate1.format(time);
	    logger.info("--------time-------" + entryDate);
//////////////////////////////////////////////////////////////////////////-------- SERVICE VARIABLES--------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
		
		String reqbeanstr = new String();
		String finalResponse = new String();
		String res_file_path = new String();
		//String refno = new String();
		String tellerno = new String();
		String branchno = new String();
		String option_code = new String();
		String pfno = new String();
		String noOfRecords = new String();
		String cifno = new String();
		String mobno = new String();
		String serviceno = serviceid;
		String sourceId  = new String();
		String validChkdFlag = new String();
		List<String> chkdRes = new ArrayList<>();
		String out_response = new String();
		String errno = new String();
		String errDesc = new String();
		List<String> errList = new ArrayList<>();
		String allowTellerBranchFlag = new String();
		
//////////////////////////////////////////////////////////////////////////-------- ENV VARIABLES--------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
		
		String callcode = "S";
		String opt = "";
		List<String> res = new ArrayList<String>();
		String formattedmasterDB1 = StringUtils.rightPad(masterDB1, 15);
		String formattedmasterDB2 = StringUtils.rightPad(masterDB2, 15);
		String lsRecArea = bancsTraceState.trim() + masterDQType.trim() + bancsHost.trim() + fnsSysnum.trim()
				+ ctrlSysnum.trim() + daySysnum.trim() + nightSysnum.trim() + non24hsum.trim() + defRMode.trim()
				+ formattedmasterDB1 + formattedmasterDB2 + servicesFlag.trim();
		String[] arr = { bancsTraceState, masterDQType, bancsHost, fnsSysnum, ctrlSysnum, daySysnum, nightSysnum,
				non24hsum, defRMode, masterDB1, masterDB2, servicesFlag, serviceid, MOB_RES_FILE_PATH, PF_RES_FILE_PATH, ErrorCodeMasterFilePath, sourceAuth, ipConfig, INSPARAM_FLAG, INSPARAM_VALUE};
		
//////////////////////////////////////////////////////////////////////////-------- INPUT VALIDATIONS--------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
		
		reqbeanstr = gson.toJson(reqbean).toString();
		tellerno = request.getHeader("TELLER_NUMBER");

	    if(reqbean != null) {
	    	
	    	if(!reqbean.isEmpty()){
	    
	    	refno	    =  reqbean.getReferenceNumber();
	    	logger.info("1refno1" + refno);
	    	branchno = request.getHeader("BRANCH_NUMBER");
	    	option_code = reqbean.getOption();
	
	    	
			errno = inpval.isNullorEmpty(arr);
		    if(!errno.equals("00000")) {
		    	if(refno == null) {
		    		refno = "";
		    	}
		    	errDesc = error.getProperty(errno);
		    	if(errDesc == null || errDesc.trim().equals("")) {
		    		logger.error("Error Description filepath not set!");
		    		finalResponse = errobj.getErrorResponse("VC015", "ERROR IN READING FILE PATH", refno);
		    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
		    	else {
				finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
				migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
				return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
		    }
		    logger.info("VC------------------------Successfully read application.properties------------------------VC");
		    errList  =  inpval.validateRefNum(refno);
		    errno = errList.get(1);
		    refno = errList.get(0);
		    if(!errno.equals("00000")) {			
		    errDesc = error.getProperty(errno);
		    if(errDesc == null || errDesc.trim().equals("")) {
	    		logger.error("Error Description Not Found!");
	    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
	    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
				return new ResponseEntity(finalResponse, HttpStatus.OK);
	    	}
	    	else {
			finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
			migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
			return new ResponseEntity(finalResponse, HttpStatus.OK);
	    	}
			}

			errList = inpval.validateTellerNum(tellerno);
		    errno = errList.get(1);
		    tellerno = errList.get(0);
			if (!errno.equals("00000")) {

				errDesc = error.getProperty(errno);
				if(errDesc == null || errDesc.trim().equals("")) {
		    		logger.error("Error Description Not Found!");
		    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
		    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
		    	else {
				finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
				migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
				return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
			}

			
			errList = inpval.validateBranchNum(branchno);
		    errno = errList.get(1);
		    branchno = errList.get(0);
			if (!errno.equals("00000")) {

				errDesc = error.getProperty(errno);
				if(errDesc == null || errDesc.trim().equals("")) {
		    		logger.error("Error Description Not Found!");
		    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
		    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
		    	else {
				finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
				migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
				return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
			}

			
			errList = inpval.validateOptionCode(option_code);
		    errno = errList.get(1);
		    option_code = errList.get(0);
			if (!errno.equals("00000")) {

				errDesc = error.getProperty(errno);
				if(errDesc == null || errDesc.trim().equals("")) {
		    		logger.error("Error Description Not Found!");
		    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
		    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
		    	else {
				finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
				migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
				return new ResponseEntity(finalResponse, HttpStatus.OK);
			}
			}

			if (option_code.equals("01")) {

				pfno = reqbean.getPFNumber();
				errList = inpval.validatePFNum(pfno);
			    errno = errList.get(1);
			    pfno = errList.get(0);
				res_file_path = PF_RES_FILE_PATH;
						
				if (!errno.equals("00000")) {

					errDesc = error.getProperty(errno);
					if(errDesc == null || errDesc.trim().equals("")) {
			    		logger.error("Error Description Not Found!");
			    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
			    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
						return new ResponseEntity(finalResponse, HttpStatus.OK);
			    	}
			    	else {
					finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
					migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
			    	}
				}
			logger.info("VC------------------------Fetched PF_RES_FILE_PATH------------------------VC");
			} else if (option_code.equals("02")) {
				mobno = reqbean.getMobileNumber();
				errList = inpval.validateMobileNum(mobno);
			    errno = errList.get(1);
			    mobno = errList.get(0);
				res_file_path = MOB_RES_FILE_PATH;
						
				if (!errno.equals("00000")) {
					errDesc = error.getProperty(errno);
					if(errDesc == null || errDesc.trim().equals("")) {
			    		logger.error("Error Description Not Found!");
			    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
			    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
						return new ResponseEntity(finalResponse, HttpStatus.OK);
			    	}
			    	else {
					finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
					migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
			    	}								
				}
			logger.info("VC------------------------Fetched MOB_RES_FILE_PATH------------------------VC");
			} else {
				errno = "VC005";
				errDesc = error.getProperty(errno);
				if(errDesc == null || errDesc.trim().equals("")) {
		    		logger.error("Error Description Not Found!");
		    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
		    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
		    	else {
				finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
				migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
				return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
			}

			cifno = reqbean.getCIFNumber();
			chkdRes = inpval.validateCIFNumber(cifno);
			errno = chkdRes.get(1);
			if (!errno.equals("00000")) {
				if (errno.equals("VC011")) {
					errDesc = error.getProperty(errno);
					if(errDesc == null || errDesc.trim().equals("")) {
			    		logger.error("Error Description Not Found!");
			    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
			    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
						return new ResponseEntity(finalResponse, HttpStatus.OK);
			    	}
			    	else {
					finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
					migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
			    	}
				} else {
					/////////////////validChkdFlag = "N";
					errDesc = errdesc.getCbsErrDesc(errno, callcode, opt, lsRecArea);
					finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
					migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
				}
			} else {
				cifno = chkdRes.get(0);
				validChkdFlag = "Y";
			}
			String requestedNoOfRecords = reqbean.getRecordCount();
		    errList = inpval.validateRecordCount(requestedNoOfRecords);	    
		    requestedNoOfRecords = errList.get(0);
		    errno = errList.get(1);
			if (!errno.equals("00000")) {
				errDesc = error.getProperty(errno);
				if(errDesc == null || errDesc.trim().equals("")) {
		    		logger.error("Error Description Not Found!");
		    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
		    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
		    	else {
				finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
				migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
				return new ResponseEntity(finalResponse, HttpStatus.OK);
			}
			}
		    
		    String lastrow = reqbean.getLastRow();
		    errList = inpval.validateLastRow(lastrow);	    
		    lastrow = errList.get(0);
		    errno = errList.get(1);
			if (!errno.equals("00000")) {
				errDesc = error.getProperty(errno);
				if(errDesc == null || errDesc.trim().equals("")) {
		    		logger.error("Error Description Not Found!");
		    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
		    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
		    	else {
				finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
				migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
				return new ResponseEntity(finalResponse, HttpStatus.OK);
			}
			}
		   
			callCode.setJvmLsCallCode(callcode);
			commonarea.setJvmLsEnvRecordArea(lsRecArea);
			jvmutenv.JVMUTENV(callCode, optcode, commonarea);

			migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "", Base64.getEncoder().encodeToString(("").getBytes()), "" );

			
			if (accept != null && accept.contains("application/json")) {
				logger.info("VC------------------------Initiating DB Connection------------------------VC");
				String dbConnection_flag = dbconnection.getCBSDBConnection("01", callcode, opt, lsRecArea);
				if (dbConnection_flag.equals("Y")) {

					logger.info(
							"VC------------------------DB Connection Fetched Sucessfully------------------------VC");
					if (validChkdFlag.equals("Y")) {
						logger.info(
								"VC------------------------Intiating Teller&Branch Validation------------------------VC");

						List<String> TellerBranchRes = validate.validateTellerBranchService(tellerno, branchno, serviceno);
						allowTellerBranchFlag = TellerBranchRes.get(0);
						errno = TellerBranchRes.get(1);
						if (allowTellerBranchFlag.equals("Y")) {

							logger.info(
									"VC------------------------Teller&Branch Validated Sucessfully------------------------VC");

							if (option_code.equals("01")) {

								logger.info(
										"VC------------------------Validated PF Option_Code------------------------VC");
								res = pfchild.callPFEnquiryChild(branchno, tellerno, pfno, cifno, Integer.parseInt(requestedNoOfRecords), lastrow);
								out_response = res.get(0);
								noOfRecords = res.get(1);
								logger.info("VC------------------------Fetched NO_OF_RECORDS------------------------VC" + noOfRecords);
								errno = res.get(2);
								logger.info("VC------------------------Fetched ERROR_NUMBER------------------------VC" + errno);
								

							} else if (option_code.equals("02")) {

								logger.info(
										"VC------------------------Validated Mobile Option_Code------------------------VC");

								res = mobchild.callMOBEnquiryChild(branchno, tellerno, mobno, cifno, Integer.parseInt(requestedNoOfRecords));
								out_response = res.get(0);
								noOfRecords = res.get(1);
								logger.info("VC------------------------Fetched NO_OF_RECORDS------------------------VC" + noOfRecords);
								errno = res.get(2);
								logger.info("VC------------------------Fetched ERROR_NUMBER------------------------VC" + errno);
								
							}
						}
					} else {
						errno = "VC011";
						errDesc = errdesc.getCbsErrDesc(errno, callcode, opt, lsRecArea);
						finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
						migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
						return new ResponseEntity(finalResponse, HttpStatus.OK);
					}
				} else {
					errno = "VC010";
					errDesc = error.getProperty(errno);
					if(errDesc == null || errDesc.trim().equals("")) {
			    		logger.error("Error Description Not Found!");
			    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
			    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );					
						return new ResponseEntity(finalResponse, HttpStatus.OK);
			    	}
			    	else {
					finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
					migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
			    	}
				}
			}

			else {
				errno = "VC009";
				errDesc = error.getProperty(errno);
				if(errDesc == null || errDesc.trim().equals("")) {
		    		logger.error("Error Description Not Found!");
		    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
		    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
		    	else {
				finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
				migDetails.saveInitialRequest(Base64.getEncoder().encodeToString((reqbeanstr.toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
				return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
			}
			}
		
		else {
			errno = "VC001";
			errDesc = error.getProperty(errno);
			if(errDesc == null || errDesc.trim().equals("")) {
	    		logger.error("Error Description Not Found!");
	    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
	    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString(("".toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
				return new ResponseEntity(finalResponse, HttpStatus.OK);
	    	}
	    	else {
			finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
			logger.info("++++++++++++++++++++++++"+Base64.getEncoder().encodeToString(("".toString()).getBytes()));
			migDetails.saveInitialRequest(Base64.getEncoder().encodeToString(("".toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
			return new ResponseEntity(finalResponse, HttpStatus.OK);
	    	}
		}
	    	}

	    	else {
	    		errno = "VC001";
				errDesc = error.getProperty(errno);
				if(errDesc == null || errDesc.trim().equals("")) {
		    		logger.error("Error Description Not Found!");
		    		finalResponse = errobj.getErrorResponse("VC014", "JAVA ERROR DESCRIPTION NOT FOUND", refno);
		    		migDetails.saveInitialRequest(Base64.getEncoder().encodeToString(("".toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
					return new ResponseEntity(finalResponse, HttpStatus.OK);
		    	}
		    	else {
				finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
				migDetails.saveInitialRequest(Base64.getEncoder().encodeToString(("".toString()).getBytes()), remoteAddress, tellerno, refno, sourceId, entryDate, IPAddress, errno, errDesc, "1", Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure" );
				return new ResponseEntity(finalResponse, HttpStatus.OK);
	    	}
	    		}

		if (errno.equals("0000") || (errno.equals("0868") && !out_response.trim().isEmpty()) || (errno.equals("0188") && !out_response.trim().isEmpty())) {
			errno = "0000";
			finalResponse = responseObj.getResponse(res_file_path, out_response, refno, noOfRecords);
			logger.info("++response++" + finalResponse);
			migDetails.updateResponse("0", errno, "OK SUCCESS",
					Base64.getEncoder().encodeToString((finalResponse).getBytes()), "success", entryDate, refno);
		} else {
			errDesc = errdesc.getCbsErrDesc(errno, callcode, opt, lsRecArea);
			finalResponse = errobj.getErrorResponse(errno, errDesc, refno);
			logger.info("++response++" + finalResponse);
			migDetails.updateResponse("1", errno, errDesc,
					Base64.getEncoder().encodeToString((finalResponse).getBytes()), "failure", entryDate, refno);
		}
		
		//logger.info("++response++" + finalResponse);
		return new ResponseEntity(finalResponse, HttpStatus.OK);

  }
// Graceful shutdown of the executor
public void shutdown() {
    singleThreadExecutor.shutdown();
    try {
        if (!singleThreadExecutor.awaitTermination(60, TimeUnit.SECONDS)) {
            singleThreadExecutor.shutdownNow();
        }
    } catch (InterruptedException ex) {
        singleThreadExecutor.shutdownNow();
    }
}
}
