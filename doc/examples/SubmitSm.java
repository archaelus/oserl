private void submit() {
}

public String  submit(String mobileNumber, String userMessage) {
    debug.enter(this, "SMPPTest.submit()");        
    try {
        if (!bound) {
            //System.out.println("Binding to SMSC......");
            bind();
        }
        SubmitSM request = new SubmitSM();
        SubmitSMResp response;
   
        // input values
        serviceType = getParam("Service type", serviceType);
        sourceAddress = getAddress("Source",sourceAddress);
        //System.out.println("Source Address : " + sourceAddress);
        //destAddress = getAddress("Destination",destAddress);
 
        destAddress = getAddress("Destination",new Address(mobileNumber));
    
        replaceIfPresentFlag = getParam("Replace if present flag", 
                                        replaceIfPresentFlag);
        //shortMessage = getParam("The short message", shortMessage);
        shortMessage = userMessage;
        scheduleDeliveryTime = getParam("Schedule delivery time", 
                                        scheduleDeliveryTime);

        validityPeriod = getParam("Validity period", validityPeriod);
        esmClass = getParam("Esm class", esmClass);
        //protocolId = getParam("Protocol id", protocolId);
        //protocolId=00;
        priorityFlag = getParam("Priority flag", priorityFlag);
        registeredDelivery = getParam("Registered delivery", 
                                      registeredDelivery);
        //dataCoding = getParam("Data encoding", dataCoding);
        //dataCoding = 0x10; // For Flash Message
        smDefaultMsgId = getParam("Sm default msg id", smDefaultMsgId);

        //System.out.println ("\nRegistered_Delivery : "+registeredDelivery);
        // set values
        request.setServiceType(serviceType);
        request.setSourceAddr(sourceAddress);
        request.setDestAddr(destAddress);
        request.setReplaceIfPresentFlag(replaceIfPresentFlag);
        request.setShortMessage(shortMessage);
        request.setScheduleDeliveryTime(scheduleDeliveryTime);
        request.setValidityPeriod(validityPeriod);
        request.setEsmClass(esmClass);
        request.setProtocolId(protocolId);
        request.setPriorityFlag(priorityFlag);
        request.setRegisteredDelivery(registeredDelivery);
        request.setDataCoding(dataCoding);
        request.setSmDefaultMsgId(smDefaultMsgId);
        // send the request
        int count = 1;
        System.out.println();
        count = getParam("How many times to submit this message (load test)", 
                         count);

        for (int i = 0; i<count; i++) {
            request.assignSequenceNumber(true);
            //System.out.print("#"+i+"  ");
            System.out.println("Submit request " + request.debugString());
            if (asynchronous) {
                //session.submit(request);  
                response = session.submit(request);
                messageId = response.getMessageId();
                //System.out.println();
                return messageId;
            } else {
                response = session.submit(request);                    
                messageId = response.getMessageId();
                System.out.println("Submit response " + 
                                   response.debugString() + 
                                   " MESSAGE ID : " + 
                                   messageId);
                //System.out.println("Submit response message id and Before Query " + messageId);
                return messageId;
            }
        }    
        return messageId;            
    } catch (Exception e) {
        event.write(e,"");
        debug.write("Submit operation failed. " + e);
        //System.out.println("Submit operation failed. " + e);
        bind();
        return messageId;
    } finally {
        debug.exit(this);
    }
}
