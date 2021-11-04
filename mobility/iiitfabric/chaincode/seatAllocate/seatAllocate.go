
/*
i) Write code in the chaincode folder of iiitfabric
ii) go get -u github.com/hyperledger/fabric/core/chaincode/shim
iii) go build
iv) cd chaincode-docker-devmode in three terminals
v) Terminal-I --> docker-compose -f docker-compose-simple.yaml up
vi) Terminal-II --> docker exec -it chaincode bash
								--> cd sacc; go build
								--> CORE_PEER_ADDRESS=peer:7052 CORE_CHAINCODE_ID_NAME=mycc:0 ./seatAllocate


vii) Terminal III --> docker exec -it cli bash
				INSTALL 						--> peer chaincode install -p chaincodedev/chaincode/seatAllocate -n mycc -v 0
				INSTANTIATE					--> peer chaincode instantiate -n mycc -v 0 -c '{"Args":["a","10"]}' -C myc
														--> -n chaincode name; -p policy name -v verion name -c constructor message (with arguments)
				Invoke							--> peer chaincode invoke -n mycc -c '{"Args":["allocate", "a", "20"]}' -C myc

				INSTALL 						--> peer chaincode install -p chaincodedev/chaincode/seatAllocate -n mycc -v 0
				Instantiate					--> peer chaincode instantiate -n mycc -v 0 -c '{"Args":[" "]}' -C myc
				Invoke							--> peer chaincode invoke -n mycc -c '{"Args":["initLedger", " "]}' -C myc

*/


package main

/* Imports
* 4 utility libraries for handling bytes, reading and writing JSON,
formatting, and string manipulation
* 2 specific Hyperledger Fabric specific libraries for Smart Contracts
*/
import (
	"bytes"
	"encoding/json"
	"fmt"
	"strconv"

	"github.com/hyperledger/fabric/core/chaincode/shim"
	sc "github.com/hyperledger/fabric/protos/peer"
)

// Define the Smart Contract structure
type SmartContract struct {
}

/* Define Seat structure, with 4 properties.
Structure tags are used by encoding/json library
*/
type Seat struct {
	; 
	; 
	; 
}

/*
 * The Init method *
 -- see initLedger()
 */
func (s *SmartContract) Init(APIstub shim.ChaincodeStubInterface) sc.Response {
	return shim.Success(nil)
}

/*
 * The Invoke method is called when an application requests to run the Smart Contract "Seat-chaincode"
 
 */
func (s *SmartContract) Invoke(APIstub shim.ChaincodeStubInterface) sc.Response {

	// Retrieve the requested Smart Contract function and arguments
	function, args := APIstub.GetFunctionAndParameters()
	// Route to the appropriate handler function to interact with the ledger
	if function == "querySeat" {
		return s.querySeat(APIstub, args)
	} else if function == "initLedger" {
		return s.initLedger(APIstub)
	} else if function == "recordSeat" {
		return s.recordSeat(APIstub, args)
	} else if function == "queryAllSeat" {
		return s.queryAllSeat(APIstub)
	} else if function == "changeSeatHolder" {
		return s.changeSeatHolder(APIstub, args)
	}

	return shim.Error("Invalid Function")
}

/*
 * The querySeat method *
 */

/*
 * The initLedger method - Adds data to the TrustlessTrust network
 */


/*
 * main function -- calls the Start function
The main function starts the chaincode in the container during instantiation.
 */
func main() {

	// Create a new Smart Contract
	err := shim.Start(new(SmartContract))
	if err != nil {
		fmt.Printf("Error creating new Smart Contract: %s", err)
	}
}

