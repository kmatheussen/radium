export interface Env {
	EMAIL: {
		send(message: {
			to: string;
			from: string;
			subject: string;
			text?: string;
		}): Promise<{ messageId: string }>;
	};
	CRASHREPORT_SECRET: string;
}

const DESTINATION_EMAIL = "k.s.matheussen@gmail.com";
const SENDER_EMAIL = "crashreport@radium.dog";
const SECRET_HEADER = "X-Radium-Secret";

const MARKER = "\n<br>";

function stripTrailingIncomplete(encoded: string): string {
	let s = encoded;
	for (;;) {
		const m = s.match(/(%[0-9A-Fa-f]{0,1})$/);
		if (!m || m[1].length === 3) break;
		s = s.slice(0, s.length - m[1].length);
	}
	return s;
}

function decodeCrashData(encoded: string): string {
	const clean = stripTrailingIncomplete(encoded);
	try {
		return decodeURIComponent(clean);
	} catch {
		return clean;
	}
}

const MAX_EMAIL_ATTEMPTS = 3;

async function sendEmailWithRetry(env: Env, text: string): Promise<unknown> {
	for (let attempt = 1; attempt <= MAX_EMAIL_ATTEMPTS; attempt++) {
		try {
			await env.EMAIL.send({
				to: DESTINATION_EMAIL,
				from: SENDER_EMAIL,
				subject: "Radium crash report " + new Date().toISOString(),
				text,
			});
			return undefined;
		} catch (error) {
			const code = error instanceof Error ? (error as { code?: string }).code ?? error.message : String(error);
			console.error(`Email send failed (attempt ${attempt}/${MAX_EMAIL_ATTEMPTS}):`, code);
			if (attempt < MAX_EMAIL_ATTEMPTS) {
				await new Promise((resolve) => setTimeout(resolve, attempt * 1000));
			} else {
				return error;
			}
		}
	}
}

function splitBody(body: string): { report: string; userText: string } {
	const idx = body.indexOf(MARKER);

	let dataPart: string;
	let userText: string;

	if (idx === -1) {
		dataPart = body;
		userText = "";
	} else {
		dataPart = body.substring(0, idx);
		userText = body.substring(idx + MARKER.length);
	}

	let report: string;
	if (dataPart.startsWith("data=")) {
		const encoded = dataPart.substring("data=".length);
		report = decodeCrashData(encoded);
	} else {
		report = dataPart;
	}

	return { report, userText };
}

function logReportDiagnostics(report: string, userText: string): void {
	const text = userText ? report + "\n\n" + userText : report;
	let controls = 0;
	let surrogates = 0;
	for (const ch of text) {
		const c = ch.codePointAt(0) ?? 0;
		if (c < 0x20 && c !== 0x09 && c !== 0x0a && c !== 0x0d) controls++;
		if (c >= 0xd800 && c <= 0xdfff) surrogates++;
	}
	console.log("report length:", text.length, "controls:", controls, "lone surrogates:", surrogates);
	console.log("report preview:", text.slice(0, 2000));
}

export default {
	async fetch(request: Request, env: Env): Promise<Response> {
		if (request.method !== "POST") {
			return new Response("Method Not Allowed", { status: 405 });
		}

		const secret = request.headers.get(SECRET_HEADER);
		if (!secret || secret !== env.CRASHREPORT_SECRET) {
			return new Response("Unauthorized", { status: 401 });
		}

		const rawBody = await request.text();

		const { report, userText } = splitBody(rawBody);

		logReportDiagnostics(report, userText);

		const text = userText ? report + "\n\n" + userText : report;

		const lastError = await sendEmailWithRetry(env, text);

		if (lastError) {
			const code = lastError instanceof Error ? (lastError as { code?: string }).code ?? lastError.message : String(lastError);
			console.error("Email send failed:", code);
			return new Response(JSON.stringify({ success: false, error: code }), {
				status: 500,
				headers: { "Content-Type": "application/json" },
			});
		}

		return new Response(JSON.stringify({ success: true }), {
			headers: { "Content-Type": "application/json" },
		});
	},
};
