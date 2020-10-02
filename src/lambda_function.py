import logging

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


def lambda_handler(event, context):
    logger.info(f'Event is {event}')

    if event:
        logger.info(f'Successfully processed event')

        try:
            event['name']
            name = event['name']
            output_string = 'My name is {}'.format(name.capitalize())

        except KeyError:
            output_string = 'A name was not defined in the event payload'

    return output_string
