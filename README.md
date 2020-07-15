Task: You should code an incomplete TCP server using MtProto protocol with just 2 states.

Server must be able to handle 'req_pq' and 'req_DH_params' requests.

req_pq answer should be a res_pq response with random data. After req_DH_params the connection should be closed.

The project should use scodec, ZIO (for managing state via Ref[A]), and java NIO2 and must use random data for simplicity.

You should use scodec.codecs.ascii32 for TL string serialization. The cipher needed for req_DH_Params is RSA/ECB/NoPadding.

You can find documentation for MtProto protocol at:

https://core.telegram.org/mtproto/auth_key

https://core.telegram.org/mtproto/samples-auth_key
