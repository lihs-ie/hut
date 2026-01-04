import { URL } from "~domains/common/uri"
import { Factory } from "~tests/factories"

export type URLProperties = {
  value: string
}

export const URLFactory = Factory<URL, URLProperties>({
  prepare: (
    overrides: Partial<URLProperties>,
    seed: number
  ): URLProperties => ({
    value: overrides.value ?? `https://example.com/${seed}`
  }),
  instantiate: (properties: URLProperties): URL => URL(properties),
  retrieve: (instance: URL): URLProperties => ({
    value: instance.value
  })
})
