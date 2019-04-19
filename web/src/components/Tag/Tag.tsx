import React       from 'react';
import classnames  from 'classnames';
import styles      from './Tag.module.css';

interface Props {
    icon:           string;
    name:           string;
    value:          string;
    strikethrough?: boolean;
    new?:           boolean;
    onIcon:         () => void;
}

const classes = (props: Props) => classnames({
    [styles.Root]:                true,
    [styles.Root__strikethrough]: props.strikethrough,
    [styles.Root__new]:           props.new
})

const Tag = (props: Props) =>
    <div className={classes(props)}>
        <span>
            <i className={`icofont-${props.icon}`} onClick={() => props.onIcon && props.onIcon()}/>
            {props.name}
        </span>

        <span>
            {props.value}
        </span>
    </div>;

export default Tag;
