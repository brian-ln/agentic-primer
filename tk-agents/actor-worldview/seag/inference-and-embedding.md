# Inference and Embedding Needs

## Inference

We need a protocol that allows us to do inference consistently across model providers.
Actors that allow us to use model versions for any given provider.
These may be 1 actor instance per model, with the same actor "code" or different actor "code"?
It isn't clear if inference actors should be stateful and maintain the session state or stateless. We might want to think about both options. Lets assume stateless now and require all inputs to generate an output.
We need to be able to control the model parameters (temperature, max tokens, ...) on inference. I don't know if this per call?
Can we take the same "conversation" and swap out models/versions or even providers?

How do libraries like Vercel's ai sdk or other "provider wrapper" services or apis think about things? What protocols exist in the wild for these providers over HTTP?

## Embedding

Same thing...


## Providers

Google Vertex (and the gemini express endpoints)
Amazon Bedrock
Microsoft Azure AI
Anthropic Claude 1st party
OpenAI 1st party
Hugging Face?
LM Studio
Other local model runners (Ollama, ...)

## Responses and Thoughts

<!-- Put your findings here -->
