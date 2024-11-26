package com.tcs.bancs.microservices.services;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.tcs.bancs.microservices.jvm.balenq.JVMAVBL;
import com.tcs.bancs.microservices.jvm.balenq.Ls400InputAccountNumber;
import com.tcs.bancs.microservices.jvm.balenq.Ls400InpEnqType;
import com.tcs.bancs.microservices.jvm.balenq.Ls400OutputErrorNumber;
import com.tcs.bancs.microservices.jvm.balenq.Ls400OutputResponse;

public class BalanceEnqService_Child {
	Logger logger = LoggerFactory.getLogger(AccountEnqService_Child.class);

	JVMAVBL jvmavbl = new JVMAVBL();

	public List<String> Balance_Enquiry_Child(String accno, int requestedNumberOfRecords) throws InterruptedException, ExecutionException {

		logger.info("VC------------------------AVALABLE BALANCE Child Service Started------------------------VC");

        String lacctno = accno == null ? "0000000000000000" : String.format("%016d", new BigInteger(accno));

        int numberOfRecordsToFetch = requestedNumberOfRecords > 0 ? requestedNumberOfRecords : 30;

        List<CompletableFuture<ResponseData>> futures = new ArrayList<>();

        ForkJoinPool customThreadPool = new ForkJoinPool(25);

        int fetchedRecordsCount = 0;

        StringBuilder allRecordsBuilder = new StringBuilder();

        while (fetchedRecordsCount < numberOfRecordsToFetch) {
            final String currentLacctno = lacctno;

            CompletableFuture<ResponseData> future = CompletableFuture.supplyAsync(() -> {
            

            	Ls400InputAccountNumber input1 = new Ls400InputAccountNumber();
            	Ls400InpEnqType input2 = new Ls400InpEnqType();
            	Ls400OutputErrorNumber output1 = new Ls400OutputErrorNumber();
            	Ls400OutputResponse output2 = new Ls400OutputResponse();
                 
                input1.setLs400InputAccountNumber(currentLacctno);
                input2.setLs400InpEnqType("1");
                                
                jvmavbl.JVMAVBL(input1,input2,output1,output2);

                ResponseData responseData = new ResponseData();
                responseData.outResponse = output2.getLs400OutputResponse();
                responseData.outResponse = responseData.outResponse.trim();
                responseData.errno = output1.getLs400OutputErrorNumber();
                return responseData;
            }, customThreadPool);

            futures.add(future);
            
            ResponseData lastResult = futures.get(futures.size() - 1).get();
           
            lacctno = lastResult.lacctno;

            String outResponse = lastResult.outResponse;
            int startIndex = 0;
            int endIndex = 0;
            if (lastResult.outResponse == null || lastResult.outResponse.trim().isEmpty()) {
                logger.error("No outResponse from JVMAVBL. Terminating process.");
                 break;
             }
            
            while (startIndex + 97 <= outResponse.length()) {
                endIndex = startIndex + 97;
                allRecordsBuilder.append(outResponse.substring(startIndex, endIndex));
                startIndex = endIndex;
                fetchedRecordsCount++;
            }
        }

        List<String> res = new ArrayList<>();
        
        res.add(allRecordsBuilder.toString());
        res.add(String.valueOf(fetchedRecordsCount));
        res.add(futures.get(futures.size() - 1).get().errno); 

        logger.info("VC------------------------AVAILABLE BALANCE Child Service Ended------------------------VC");

        customThreadPool.shutdown();         
        return res;
        
    }

    private class ResponseData {
        String outResponse;
        String errno;
        String lacctno;
    }
}





package com.tcs.bancs.microservices.services;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.tcs.bancs.microservices.services.BalanceEnqService_Child.ResponseData;
import com.tcs.bancs.microservices.jvm.balenq.JVMCUCC;
import com.tcs.bancs.microservices.jvm.balenq.LsErrorNumber;
import com.tcs.bancs.microservices.jvm.balenq.LsInputCustNumber;
import com.tcs.bancs.microservices.jvm.balenq.LsOutputAccountNumbers;

public class AccountEnqService_Child {
    private static final Logger logger = LoggerFactory.getLogger(AccountEnqService_Child.class);

    private final JVMCUCC jvmcucc = new JVMCUCC();
    private final BalanceEnqService_Child balanceEnqService = new BalanceEnqService_Child();

    public List<ResponseData> fetchAccountBalances(String cifno, int requestedNumberOfRecords, String enquiryType)
            throws InterruptedException, ExecutionException {
        logger.info("VC------------------ACCOUNT NUMBERS & BALANCE ENQUIRY Child Service Started------------------VC");

        // Step 1: Fetch account numbers
        String lacctno = (cifno == null || cifno.isEmpty()) ? "00000000000000000" : String.format("%017d", new BigInteger(cifno));

        // Prepare input and output objects
        LsInputCustNumber input = new LsInputCustNumber();
        LsOutputAccountNumbers output = new LsOutputAccountNumbers();
        LsErrorNumber error = new LsErrorNumber();

        input.setLsInputCustNumber(lacctno);
        jvmcucc.JVMCUCC(input, error, output);

        String outResponse = output.getLsOutputAccountNumbers();
        if (outResponse == null || outResponse.trim().isEmpty()) {
            logger.error("No valid response from JVMCUCC.");
            return new ArrayList<>();
        }

        // Step 2: Split response into account numbers
        List<String> accountNumbers = new ArrayList<>();
        outResponse = outResponse.trim();
        for (int i = 0; i + 16 <= outResponse.length() && accountNumbers.size() < requestedNumberOfRecords; i += 16) {
            String accountNumber = outResponse.substring(i, i + 16).trim();
            if (!accountNumber.matches("0+")) { // Exclude records with only zeros
                accountNumbers.add(accountNumber);
            }
        }

        // Step 3: Pass account numbers to BalanceEnqService_Child
        List<ResponseData> balanceResults = balanceEnqService.Balance_Enquiry_Child(accountNumbers, enquiryType);

        logger.info("VC------------------ACCOUNT NUMBERS & BALANCE ENQUIRY Child Service Ended------------------VC");

        return balanceResults;
    }
}