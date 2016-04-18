#' R module for working with the 'Zabbix API'
#'
#' @description  
#' \code{ZabbixAPI} function Enables easy and direct communiaction with \code{Zabbix API} from \pkg{R}.
#' Each request to the \code{Zabbix API} requires passing \code{auth} authentication token. 
#' Generation of the authentication token is explained \href{https://www.zabbix.com/documentation/3.0/manual/api/reference/user/login}{here} or
#' can be seen in Examples section.
#'
#' @details 
#' Communication wih the \code{Zabbix API} is described at \href{https://www.zabbix.com/documentation/3.0/manual/api/reference}{Zabbix API Manual Reference} and
#' relies on specifying methods of call and additional parameters in the \href{http://www.jsonrpc.org/specification}{JSON-RPC} format. An example of such specification
#' can be found in the \href{https://www.zabbix.com/documentation/3.0/manual/api/reference/user/login#examples}{example of authenticating a user} but in \pkg{RZabbix} this
#' should be specified in parameter \code{body} where parameters are passed in the named list, where additional \code{params} should be passed in \code{jsonlite::unbox(data.frame())}.
#' See examples.
#'
#' @param server A character with base URI for zabbix web interface (omitting /api_jsonrpc.php).
#' @param user.agent,content.type Character arguments passed to \link{POST} through \link{user_agent()} and \link{content_type}.
#' @param encode The same as \code{encode} in \link{POST}.
#' @param ... Further arguments passed to \link{POST}.
#' @param only.content,fromJSON,content.only.result 
#'     \itemize{
#'       \item \code{only.content} Should return results only for \code{content} part of the \link{response}.
#'       \item \code{fromJSON} Should apply \link{fromJSON} conversion for \code{content} when \code{only.content = TRUE}.
#'       \item \code{content.only.result} Should return only \code{result} part of \code{fromJSON(rawToChar(response$content))} when \code{only.content = TRUE, fromJSON = TRUE}.
#'     }
#' @param body A named list specifying \code{method}, user \code{auth} token and additional \code{params} (passed in \code{jsonlite::unbox(data.frame())}) to be used during request.
#' Available methods are described in \href{https://www.zabbix.com/documentation/3.0/manual/api}{Zabbix Documentation}. One does not have to pass \code{id} and \code{jsonrpc}.
#'
#' @note 
#' Bug reports and feature requests can be sent to \href{https://github.com/MarcinKosinski/RZabbix/issues}{https://github.com/MarcinKosinski/RZabbix/issues}
#' 
#' 
#' @author 
#' Marcin Kosinski, \email{m.p.kosinski@@gmail.com}
#' 
#'
#' @examples
#' \dontrun{
#' # user authentication
#' ZabbixAPI('http://localhost/zabbix',
#'           body = list(method = "user.login",
#'                       params = jsonlite::unbox(
#' 	                      data.frame(user = "Admin",
#' 	                                 password = "zabbix")))) -> auth
#'																	 
#' # request to get histoy of an item of 'item_id' number																	 
#' ZabbixAPI("http://localhost/zabbix",
#'           body = list(method = "history.get",
#'                       params = jsonlite::unbox(
#'                        data.frame(output = "extend",
#'                                   itemids = "item_id",
#'                                   history = 0,
#'                                   sortfield = "clock",
#'                                   sortorder = "DESC",
#'                                   limit = 10)
#'                      ),
#'                      auth = auth))
#' 											
#' # API info
#' ZabbixAPI('http://localhost/zabbix',
#'           body = list(method = "apiinfo.version"))
#' 
#'    
#' }
#' @import httr
#' @importFrom jsonlite fromJSON
#' @export
#' @rdname ZabbixAPI
ZabbixAPI <- function(server = 'http://localhost/zabbix',
											body = list(),
											user.agent = 'RZabbix',
											content.type = 'application/json-rpc',
											encode = "json",
											...,
											only.content = TRUE,
											fromJSON = TRUE,
											content.only.result = TRUE) {
	
		url <- paste0(server, "/api_jsonrpc.php")
		
		body['jsonrpc'] <- '2.0'
		body['id'] <- get('id', envir = .RZabbixEnv)
		on.exit(assign('id', get('id', envir = .RZabbixEnv) +1, envir = .RZabbixEnv))
		
    zabbix.response <- 
    	httr::POST(url,
    						 content_type(content.type),
    						 user_agent(user.agent),
    					   encode = encode,
    					   body = body,
    					 	 verbose())
    
    if (only.content) {
    	zabbix.response <- rawToChar(zabbix.response$content)
    	if (fromJSON) {
    		zabbix.response <- fromJSON(zabbix.response)
    		if (content.only.result) {
    			zabbix.response <- zabbix.response$result
    		}
    	}
    }
    
    zabbix.response
    
}